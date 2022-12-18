-module(aoc_problem_18).
-export([solve/2]).

solve(Problem, Example) ->
    Positions = aoc_timer:input(fun ingest/2, [Problem, Example]),
    aoc_timer:problem(1, fun solve_1/1, [Positions]),
    aoc_timer:problem(2, fun solve_2/1, [Positions]).

ingest(Problem, Example) ->
    lists:map(fun (Line) -> [A,B,C] = lists:map(fun erlang:binary_to_integer/1, binary:split(Line, <<",">>,[global])), {A,B,C} end, aoc_input:split_by_lines(Problem, Example)).

solve_1(Positions) ->
    Set = sets:from_list(Positions),
    lists:foldl(fun (Pos, Count) -> Count + count_exposed_faces(Pos, Set) end, 0, Positions).

count_exposed_faces(Pos, Set) ->
    Check = adjacent_3d(Pos),
    % io:format("For ~p check ~p~n", [Pos, Check]),
    Exposed = lists:filter(fun (P) -> not sets:is_element(P, Set) end, Check),
    length(Exposed).

adjacent_3d(Pos) ->
    lists:flatten(adjacent_3d_helper(Pos, 1)).
adjacent_3d_helper(_, 4) ->
    [];
adjacent_3d_helper(Pos, N) ->
    Nth = element(N, Pos),
    [setelement(N, Pos, Nth+1), setelement(N, Pos, Nth-1), adjacent_3d_helper(Pos, N+1)].

solve_2(Positions) ->
    Set = sets:from_list(Positions),
    lists:foldl(fun (Pos, Count) -> Count + count_exposed2_faces(Pos, Set) end, 0, Positions).

count_exposed2_faces(Pos, Set) ->
    Check = adjacent_3d(Pos),
    % io:format("For ~p check ~p~n", [Pos, Check]),
    Exposed = lists:filter(fun (P) ->
        case not sets:is_element(P, Set) of 
            true ->
                %% Doesn't work: needs to handle bubbles larger than 1x1x1
                AirFaces = count_exposed_faces(P, Set),
                % io:format("~p is clear and has ~p faces~n", [P, AirFaces]),
                AirFaces > 0;
            false ->
                false
        end
    end, Check),
    length(Exposed).
