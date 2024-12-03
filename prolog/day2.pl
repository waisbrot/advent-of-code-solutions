:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).

input(ex2, Data) :-
  parsed('../inputs/02ex.txt', Data).
input(in2, Data) :-
  parsed('../inputs/02.txt', Data).
  
parsed(File, Data) :-
  phrase_from_file(level_reports(Data), File).

solve(day2a, Input, Answer) :-
  solve_day_2(levels_are_safe, Input, Answer).
solve(day2b, Input, Answer) :-
  solve_day_2(levels_damp_safe, Input, Answer).
  
solve_day_2(Pred, Input, Answer) :-
  input(Input, Parsed),
  include(Pred, Parsed, Safe),
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

levels_damp_safe(report([_])).
levels_damp_safe(report([A,B|Rest])) :-
  A > B,
  levels_damp_safe('>', report([A,B|Rest])).
levels_damp_safe(report([A,B|Rest])) :-
  A < B,
  levels_damp_safe('<', report([A,B|Rest])).
levels_damp_safe(report([A,B|Rest])) :-
  levels_are_safe(report([A|Rest]));
  levels_are_safe(report([B|Rest])).

levels_damp_safe(_, report([_])).
levels_damp_safe(LG, report([A,B|Rest])) :-
  call(LG, A, B),
  abs(A - B, Delta),
  Delta >= 1,
  Delta =< 3,
  levels_damp_safe(LG, report([B|Rest])).
levels_damp_safe(LG, report([A,_|Rest])) :-
  levels_are_safe(LG, report([A|Rest])).

