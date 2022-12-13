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
    ingest_rows(0, Lines, #{}).

ingest_rows(_Row, [], Terrain) ->
    Terrain;
ingest_rows(Row, [Line|Lines], Terrain) ->
    Terrain2 = ingest_row(Row, 0, binary_to_list(Line), Terrain),
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
    ets:new(open_set, [ordered_set, private, named_table]),
    ets:new(solutions, [ordered_set, private, named_table]),
    {Initial} = with_heuristic(Start, sets:new(), Finish),
    a_star(Initial, Terrain).

solve_2(_) ->
    ok.

manattan_distance({R1, C1}, {R2, C2}) ->
    abs(R1 - R2) + abs(C1 - C2).

with_heuristic(Pos, Visited, Dest) ->
    Hope = manattan_distance(Pos, Dest),
    Regret = sets:size(Visited),
    {{Hope + Regret, Pos, Visited, Dest}}.

a_star('$end_of_table', _) ->
    ?LOG_DEBUG("Open set empty"),
    io:format("Open set=~p~n", [ets:tab2list(open_set)]),
    ets:last(solutions);
a_star(Top, Terrain) ->
    ?LOG_DEBUG(fun print_partial/1, ["Evaluating partial solution: ", Top]),
    ets:delete_object(open_set, {Top}),
    Children = generate_children(Top, Terrain),
    {Solutions, InProgress} = lists:partition(fun is_solution/1, Children),
    ?LOG_DEBUG("Solutions=~p InProgress=~p Already in progress=~p", [length(Solutions), length(InProgress), length(ets:tab2list(open_set))]),
    lists:foreach(fun (S) -> ets:insert(solutions, S) end, Solutions),
    lists:foreach(fun (P) -> ets:insert(open_set, P) end, InProgress),
    Next = ets:first(open_set),
    a_star(Next, Terrain).

print_partial([Message, {Heuristic, Pos, Visited, _}]) ->
    io_lib:format("~s ~p", [Message, {Heuristic, Pos, sets:size(Visited)}]).

is_solution({_, Finish, _, Finish}) -> true;
is_solution(_) -> false.

generate_children({_, Pos, Visited, Dest}, Terrain) ->
    Neighbors = all_neighbors(Pos, Visited, Terrain),
    Visited2 = sets:add_element(Pos, Visited),
    lists:map(fun (N) -> with_heuristic(N, Visited2, Dest) end, Neighbors).

all_neighbors({R, C} = From, Visited, Terrain) ->
    Possible = [{R+1, C}, {R-1, C}, {R, C+1}, {R, C-1}],
    NotVisited = lists:filter(fun (Pos) -> not sets:is_element(Pos, Visited) end, Possible),
    OnMap = lists:filter(fun (Pos) -> maps:is_key(Pos, Terrain) end, NotVisited),
    lists:filter(fun (Pos) -> can_climb(From, Pos, Terrain) end, OnMap).

can_climb(From, To, Terrain) ->
    FromH = maps:get(From, Terrain),
    ToH = maps:get(To, Terrain),
    abs(FromH - ToH) =< 1.
