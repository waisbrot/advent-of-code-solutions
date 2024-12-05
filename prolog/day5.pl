:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).
:- use_module(library(portray_text)).
:- use_module(library(clpfd)).

:- dynamic page_order/2.
:- retractall(page_order(_,_)).

input(Part, ex5, Data) :-
  parsed(Part, '../inputs/05ex.txt', Data).
input(Part, in5, Data) :-
  parsed(Part, '../inputs/05.txt', Data).

solve(day5a, Input, Answer) :-
  instantiate(a, Input, Prints),
  include(ordered_printing, Prints, Ordered),
  maplist(middle_page, Ordered, Middles),
  sum_list(Middles, Answer).
solve(day5b, Input, Answer) :-
  instantiate(a, Input, Prints),
  exclude(ordered_printing, Prints, Unordered),
  maplist(sorted_printing, Unordered, Ordered),
  maplist(middle_page, Ordered, Middles),
  sum_list(Middles, Answer).

instantiate(Part, Input, Print) :-
  retractall(page_order(_,_)),
  input(Part, Input, [Order,Print]),
  assert_all(Order).

assert_all([Loc|Rest]) :-
  assertz(Loc),
  assert_all(Rest).
assert_all([]).


parsed(a, File, Data) :-
  set_portray_text(enabled, true),
  phrase_from_file(problem_parts(page_ordering(O), page_printings(P)), File),
  set_portray_text(enabled, false),
  Data = [O, P].
  
problem_parts(page_ordering(Ordering), page_printings(Printing)) -->
  page_ordering(Ordering),
  page_printings(Printing).
  
page_ordering([page_order(Before,After)|T]) -->
  integer(Before),"|",integer(After),"\n",
  page_ordering(T).
page_ordering([]) -->
  "\n", [].

page_printings([page_printing(H)|T]) -->
  page_printing(H),
  page_printings(T).
page_printings([]) --> [].

page_printing([H|T]) -->
  integer(H), ",", page_printing(T).
page_printing([H]) -->
  integer(H), eol.

page_order(A,B) :-
  page_order(B,A), !, fail.
page_order(A,C) :-
  page_order(A,B),
  page_order(B,C).

ordered_printing(P) :-
  sorted_printing(P, P).

middle_page(page_printing(P), Middle) :-
  length(P, Len),
  Half is Len/2,
  floor(Half, Index),
  Half =\= Index,
  nth0(Index, P, Middle).

sorted_printing(page_printing(P), page_printing(Po)) :-
  predsort(compare_pages, P, Po).
  
compare_pages('<', P0, P1) :- page_order(P0, P1).
compare_pages('>', P0, P1) :- page_order(P1, P0).
compare_pages('=', P, P).

