:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).
:- use_module(library(portray_text)).
:- use_module(library(clpfd)).
:- set_portray_text(enabled, X, X).

:- dynamic letter/3.
:- retractall(letter(_,_,_)).

input(Part, ex4, Data) :-
  parsed(Part, '../inputs/04ex.txt', Data).
input(Part, in4, Data) :-
  parsed(Part, '../inputs/04.txt', Data).

solve(day4a, Input, Answer) :-
  instantiate(a, Input),
  aggregate(count, [X,S], xmas(X,S), Answer).
solve(day4b, Input, Answer) :-
  instantiate(a, Input),
  aggregate(count, [C], x_mas(C), Answer).
  
instantiate(Part, Input) :-
  retractall(letter(_,_,_)),
  input(Part, Input, Grid),
  assert_all(Grid).
  
xmas(x(XR,XC),s(SR,SC)) :-
  letter(x, XR, XC),
  RDelta #>= -1, RDelta #=< 1,
  CDelta #>= -1, CDelta #=< 1,
  MR #= XR + RDelta,
  MC #= XC + CDelta,
  letter(m, MR, MC),
  AR #= MR + RDelta,
  AC #= MC + CDelta,
  letter(a, AR, AC),
  SR #= AR + RDelta,
  SC #= AC + CDelta,
  letter(s, SR, SC).
  
x_mas(center(AR,AC)) :-
  letter(a, AR, AC),

  TLR #= AR - 1,
  TLC #= AC - 1,
  letter(TL, TLR, TLC),

  TRR #= AR - 1,
  TRC #= AC + 1,
  letter(TR, TRR, TRC),
  
  BLR #= AR + 1,
  BLC #= AC - 1,
  letter(BL, BLR, BLC),
  
  BRR #= AR + 1,
  BRC #= AC + 1,
  letter(BR, BRR, BRC),
  
  (
    TL = m, BR = s;
    TL = s, BR = m
  ),
  (
    TR = m, BL = s;
    TR = s, BL = m
  ).
  
parsed(a, File, Data) :-
  phrase_from_file(letter_grid(Data), File).

assert_all([Loc|Rest]) :-
  assertz(Loc),
  assert_all(Rest).
assert_all([]).

letter_grid(Grid) -->
  letter_grid_(loc(0,0),Grid),!.
letter_grid_(loc(_,_),[]) --> eos, [].
letter_grid_(loc(R0,C0),[letter(Letter,R0,C0)|Rest]) -->
  nonblank(LetterCode),
  {
    LC is LetterCode+32, % lower-case for pretty
    atom_codes(Letter, [LC]),
    R is R0,
    C is C0 + 1
  },
  letter_grid_(loc(R,C),Rest).
letter_grid_(loc(R0,_),Rest) -->
  eol,
  { R is R0 + 1, C is 0 },
  letter_grid_(loc(R,C),Rest).
  
