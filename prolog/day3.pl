:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).

input(Part, ex3, Data) :-
  parsed(Part, '../inputs/03ex.txt', Data).
input(Part, ex3b, Data) :-
  parsed(Part, '../inputs/03bex.txt', Data).
input(Part, in3, Data) :-
  parsed(Part, '../inputs/03.txt', Data).
  
solve(day3a, Input, Output) :-
  solve_helper(a, Input, Output).
solve(day3b, Input, Output) :-
  solve_helper(b, Input, Output).
  
solve_helper(Part, Input, Output) :-
  input(Part, Input, Parsed),
  maplist(mul_eval, Parsed, Products),
  sum_list(Products, Output).
  
mul_eval(mul(A,B), Res) :-
  Res is A * B.

parsed(a, File, Data) :-
  phrase_from_file(instructions(Data), File).
parsed(b, File, Data) :-
  phrase_from_file(instructions_b(Data), File).

instructions(I) -->
  "mul(",!,
  i_mul(I).
instructions(Rest) -->
  \+ "mul(",
  nonblank(_),!,
  instructions(Rest).
instructions(Rest) -->
  blank,!,
  instructions(Rest).
instructions([]) --> [].
i_mul([mul(A,B)|Rest]) -->
  integer(A),
  ",",
  integer(B),
  ")",!,
  instructions(Rest).
i_mul(Pass) -->
  instructions(Pass).

instructions_b(I) --> instructions_do(I).
instructions_do(I) -->
  "mul(",!,
  i_mul_d(I).
instructions_do(I) -->
  "don't()",!,
  instructions_do_not(I).
instructions_do(Rest) -->
  \+ "mul(",
  \+ "don't()",
  nonblank(_),!,
  instructions_do(Rest).
instructions_do(Rest) -->
  blank,!,
  instructions_do(Rest).
instructions_do([]) --> [].

instructions_do_not(I) -->
  "do()",
  instructions_do(I).
instructions_do_not(I) -->
  \+ "do()",
  nonblank(_),!,
  instructions_do_not(I).
instructions_do_not(I) -->
  blank,!,
  instructions_do_not(I).
instructions_do_not([]) --> [].

i_mul_d([mul(A,B)|Rest]) -->
  integer(A),
  ",",
  integer(B),
  ")",!,
  instructions_do(Rest).
i_mul_d(Pass) -->
  instructions_do(Pass).
