example(day1,[[3,4,2,1,3,3], [4,3,5,3,9,3]]).

solve(day1a, Input, Output) :-
  [List1,List2|[]] = Input,
  msort(List1,Sorted1),
  msort(List2,Sorted2),
  summeddifferences(Sorted1, Sorted2, Output).
solve(day1b, Input, Output) :-
  [List1, List2] = Input,
  compressed_list(List1, Comp1),
  compressed_list(List2, Comp2),
  similarity_score(Comp1, Comp2, Output).

similarity_score([], _, 0).
similarity_score([[LeftNumber, LeftCount]|Left], RightList, Score) :-
  similarity_score_helper(LeftNumber, LeftCount, RightList, ThisScore),
  similarity_score(Left, RightList, RestScore),
  Score is ThisScore + RestScore.

similarity_score_helper(_, _, [], 0).
similarity_score_helper(LeftNumber, LeftCount, [[LeftNumber, RightCount]|_], Score) :-
  Score is LeftNumber * LeftCount * RightCount.
similarity_score_helper(LeftNumber, LeftCount, [[RightNumber, _]|Right], Score) :-
  RightNumber =\= LeftNumber,
  similarity_score_helper(LeftNumber, LeftCount, Right, Score).

summeddifferences([], [], 0).
summeddifferences([H1|T1],[H2|T2], Diff) :-
  abs(H1 - H2, Res),
  summeddifferences(T1, T2, Rem),
  Diff is Rem+Res.
                                    
compressed_list([], []).
compressed_list([H|T], Comp) :-
  compressed_list(T, Rest),
  add_to_clist(H, Rest, Comp).
                        
add_to_clist(El, [], [[El,1]]).
add_to_clist(El, [[El, Old]|Rest], Result) :-
  New is Old+1,
  Result = [[El, New]|Rest].
add_to_clist(El, [H|Rest], Result) :-
  [N,_] = H,
  N =\= El,
  add_to_clist(El, Rest, RTail),
  Result = [H|RTail].

