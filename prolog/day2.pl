:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).

input(ex2, Data) :-
  parsed('../inputs/02ex.txt', Data).
input(in2, Data) :-
  parsed('../inputs/02.txt', Data).
  
parsed(File, Data) :-
  phrase_from_file(level_reports(Data), File).

solve(day2a, Input, Answer) :-
  input(Input, Parsed),
  include(levels_are_safe, Parsed, Safe),
  length(Safe, Answer).

level_reports([]) --> [].
level_reports([Report|Reports]) -->
  level_report(Report),
  "\n",!,
  level_reports(Reports).

level_report(report(Levels)) -->
  level_report_list(Levels).
level_report_list([Level|Rest]) -->
  integer(Level),
  white,!,
  level_report_list(Rest).
level_report_list([Level]) -->
  integer(Level),
  [].

levels_are_safe(report([_])).
levels_are_safe(report([A,B|Rest])) :-
  A > B,
  levels_are_safe('>', report([A,B|Rest])).
levels_are_safe(report([A,B|Rest])) :-
  A < B,
  levels_are_safe('<', report([A,B|Rest])).
levels_are_safe(_, report([_])).
levels_are_safe(LG, report([A,B|Rest])) :-
  call(LG, A, B),
  abs(A - B, Delta),
  Delta >= 1,
  Delta =< 3,
  levels_are_safe(LG, report([B|Rest])).

