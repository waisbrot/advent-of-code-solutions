-module(aoc_problem_12).
-export([solve/2]).
-include_lib("kernel/include/logger.hrl").

solve(Problem, Example) ->
    ok = logger:update_primary_config(#{level => debug}),
    %logger:add_primary_filter(debug, {fun (#{meta := #{debug := true}}, _) -> ignore; (_, _) -> stop end, ok}),
    Terrain = aoc_timer:input(fun ingest/2, [Problem, Example]),
    aoc_timer:problem(1, fun solve_1/1, [Terrain]),
    % fprof:apply(aoc_timer, problem, [2, fun solve_2/1, [Monkeys]]).
    aoc_timer:problem(2, fun solve_2/1, [Terrain]).

ingest(Problem, Example) ->
    Lines = aoc_input:split_by_lines(Problem, Example),
    ingest_rows(1, Lines, #{}).

ingest_rows(_Row, [], Terrain) ->
    Terrain;
ingest_rows(Row, [Line|Lines], Terrain) ->
    Terrain2 = ingest_row(Row, 1, binary_to_list(Line), Terrain),
    ingest_rows(Row+1, Lines, Terrain2).

ingest_row(_Row, _Col, [], Terrain) ->
    Terrain;
ingest_row(Row, Col, [H|Rest], Terrain) ->
    Terrain2 = case H of
        $S -> Terrain#{start => {Row, Col}, {Row, Col} => $a};
        $E -> Terrain#{finish => {Row, Col}, {Row, Col} => $z};
        _ -> Terrain#{{Row, Col} => H}
    end,
    ingest_row(Row, Col+1, Rest, Terrain2).

solve_1(#{start := Start, finish := Finish} = Terrain) ->
    OpenSet = [with_heuristic(Start, 0, Finish)],
    ClosedSet = [],
    BestSolution = maps:size(Terrain),
    Visited = #{},
    a_star(OpenSet, ClosedSet, Visited, BestSolution, Terrain).

solve_2(#{start := OriginalStart, finish := Finish} = Terrain0) ->
    Terrain = maps:without([start,finish], Terrain0),
    OtherStarts = lists:map(fun ({Pos,_}) -> Pos end, lists:filter(fun ({_, H}) -> H =:= $a end, maps:to_list(Terrain))),
    lists:foldl(fun (Start, Best) ->
        MaybeBetter = a_star([with_heuristic(Start, 0, Finish)], [], #{}, Best, Terrain),
        io:format("From ~p: ~p (best so far is ~p)~n", [Start, MaybeBetter, Best]),
        min(Best, MaybeBetter)
    end, maps:size(Terrain), [OriginalStart|OtherStarts]).

manattan_distance({R1, C1}, {R2, C2}) ->
    abs(R1 - R2) + abs(C1 - C2).

with_heuristic(Pos, Steps, Dest) ->
    Hope = manattan_distance(Pos, Dest),
    Regret = Steps,
    {Hope + Regret, Pos, Steps, Dest}.

a_star([], _ClosedSet, _Visited, BestSolution, _Terrain) ->
    % io:format("OpenSet=~p ClosedSet=~p Best=~p~n", [length([]), length(ClosedSet), BestSolution]),
    % io:format("Open set empty. Closed set: ~p~n", [ClosedSet]),
    % draw_visited(Visited),
    BestSolution;
a_star([Top|OpenSet], ClosedSet, Visited, BestSolution, Terrain) ->
    {Visited2, Children} = generate_children(Top, Visited, BestSolution, Terrain),
    % io:format("Visited2=~p Children=~p~n", [maps:size(Visited2), Children]),
    {Solutions, InProgress} = lists:partition(fun is_solution/1, Children),
    BestSolution2 = update_best_solution(Solutions, BestSolution),
    ClosedSet2 = ClosedSet ++ Solutions,
    OpenSet2 = sort_and_simplify(OpenSet ++ InProgress),
    % Top2 = case OpenSet2 of [H|_] -> H; _ -> none end,
    % case map_size(Visited) rem 100 of
    %     0 -> draw_visited(Visited);
    %     _ -> ok
    % end,
    % io:format("Top=~p(~c) -> ~p(~c) OpenSet=~p -> ~p ClosedSet=~p -> ~p Visited=~p -> ~p Best=~p -> ~p~n", 
    %     [Top, get_terrain(Top, Terrain), Top2, get_terrain(Top2, Terrain), length(OpenSet), length(OpenSet2)-1, length(ClosedSet), length(ClosedSet2),
    %         maps:size(Visited), maps:size(Visited2), BestSolution, BestSolution2]),
    a_star(OpenSet2, ClosedSet2, Visited2, BestSolution2, Terrain).

is_solution({_, Finish, _, Finish}) -> true;
is_solution(_) -> false.

draw_visited(V) when map_size(V) =:= 0 -> ok;
draw_visited(Visited) ->
    VList = lists:sort(maps:to_list(Visited)),
    % io:format("VList=~p~n", [VList]),
    draw_visited({1,1}, VList).
draw_visited(_, []) ->
    io:format("~n");
draw_visited({R1, C1}, [{{R2, C2},_}|Rest] = Visited) ->
    if
        R1 < R2 -> 
            io:format("|~n"),
            draw_visited({R1+1, 1}, Visited);
        C1 < C2 ->
            io:format("."),
            draw_visited({R1,C1+1}, Visited);
        C1 =:= C2 ->
            io:format("#"),
            draw_visited({R2, C2+1}, Rest)
    end.

get_terrain(none, _) -> $ ;
get_terrain({_, Pos, _, _}, Terrain) -> maps:get(Pos, Terrain).

sort_and_simplify(List) ->
    lists:uniq(fun ({Score, Pos, _, _}) -> {Score, Pos} end, lists:sort(List)).

generate_children({_, Pos, Steps, Dest}, Visited, BestSolution, Terrain) ->
    Neighbors = all_neighbors(Pos, Terrain),
    ScoredNeighbors = lists:map(fun (N) -> with_heuristic(N, Steps+1, Dest) end, Neighbors),
    BetterScore = lists:filter(fun ({NScore, NPos, _, _}) -> 
        BestForPos = maps:get(NPos, Visited, BestSolution),
        NScore < BestForPos
    end, ScoredNeighbors),
    Visited2 = lists:foldl(fun ({NScore, NPos, _, _}, Vis) -> Vis#{NPos => NScore} end, Visited, BetterScore),
    {Visited2, BetterScore}.

update_best_solution(Solutions, BestSolution) ->
    lists:min([BestSolution | lists:map(fun ({Score, _, _, _}) -> Score end, Solutions)]).

all_neighbors({R, C} = From, Terrain) ->
    Possible = [{R+1, C}, {R-1, C}, {R, C+1}, {R, C-1}],
    OnMap = lists:filter(fun (Pos) -> maps:is_key(Pos, Terrain) end, Possible),
    lists:filter(fun (Pos) -> can_climb(From, Pos, Terrain) end, OnMap).

can_climb(From, To, Terrain) ->
    FromH = maps:get(From, Terrain),
    ToH = maps:get(To, Terrain),
    (ToH - FromH) =< 1.
