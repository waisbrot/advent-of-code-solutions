-module(aoc_problem_16).
-export([solve/2]).

solve(Problem, Example) ->
    Map = aoc_timer:input(fun ingest/2, [Problem, Example]),
    aoc_timer:problem(1, fun solve_1/1, [Map]),
    aoc_timer:problem(2, fun solve_2/1, [Map]).

ingest(Problem, Example) ->
    Lines = aoc_input:split_by_lines(Problem, Example),
    {ok, RE} = re:compile("Valve ([A-Z]+) has flow rate=(\\d+); tunnels? leads? to valves? ([A-Z ,]+)", [anchored]),
    maps:from_list(lists:map(fun (Line) ->
        % io:format("Line=~p~n", [Line]),
        {match, [Valve, Flow, ExitsList]} = re:run(Line, RE, [{capture, all_but_first, binary}]),
        Exits = lists:map(fun cave_to_atom/1, binary:split(ExitsList, <<", ">>, [global])),
        {cave_to_atom(Valve), {binary_to_integer(Flow), Exits}}
    end, Lines)).

cave_to_atom(B) ->
    list_to_atom(string:to_lower(binary_to_list(B))).

solve_1(Map) ->
    MaxFlow = lists:foldl(fun ({F,_}, Sum) -> Sum + F end, 0, maps:values(Map)),
    State0 = #{
        pos => aa, 
        time => 30, 
        sum => 0,
        flow => 0, 
        open => #{},
        not_open => MaxFlow,
        path => [],
        since_last_open => #{aa => true}
    },
    OpenSet = [score_state(State0, Map)],
    io:format("Start: ~p~n", OpenSet),
    astar(OpenSet, 0, #{}, Map, 0).

score_state(#{pos := Pos, time := T, flow := F, sum := S, open := Opened} = State, Map) ->
    State#{score => (F * T) + S}.

astar([], Best, _, _, _) ->
    Best;
astar([State|OpenSet], BestSolution, BestProgress, Map, Iteration) ->
    Children = generate_children(State, Map),
    if Iteration rem 1_000 =:= 0 -> io:format("Iteration=~w State=~w BestProgress=~w Open=~w Children=~w~n", [Iteration, State, BestProgress, length(OpenSet), Children]); true -> ok end,
    {Solutions, InProgress} = lists:partition(fun is_solution/1, Children),
    BestSolution2 = update_best_solution(Solutions, BestSolution),
    if BestSolution =:= BestSolution2 -> ok; true -> io:format("Better solution found: ~p~n", [Solutions]) end,
    {OpenSet2, BestProgress2} = sort_and_simplify(OpenSet ++ InProgress, BestProgress),
    astar(OpenSet2, BestSolution2, BestProgress2, Map, Iteration+1).

generate_children(#{pos := Pos, open := Open, not_open := Remaining, since_last_open := Moves}=Current, Map) ->
    Neighbors = get_neighbors(Current, Map),
    Neighbors2 = if Remaining =:= 0 -> Neighbors; true -> lists:filter(fun (N) -> not is_map_key(N, Moves) end, Neighbors) end,
    Children = lists:map(fun (N) -> move(Current, N, Map) end, Neighbors2),
    if is_map_key(Pos, Open) -> Children; true -> [open_valve(Current, Map)|Children] end.

update_best_solution(Solutions, BestSolution) ->
    lists:max([BestSolution|lists:map(fun (#{sum := S}) -> S end, Solutions)]).

is_solution(#{time := T}) -> 
    T =:= 0.

sort_and_simplify(List, BestProgress) ->
    {NotWorse, BestProgress2} = filter_and_progress(List, BestProgress),
    {lists:sort(fun (#{score := S1}, #{score := S2}) -> S1 > S2 end, NotWorse), BestProgress2}.

filter_and_progress(List, Progress) ->
    filter_and_progress(List, Progress, []).
filter_and_progress([], Progress, Filtered) ->
    {Filtered, Progress};
filter_and_progress([State = #{time := T, not_open := Remaining, score := Score}|Rest], Progress, Filtered) ->
    NumNotOpen = length(Remaining),
    Key = {T, NumNotOpen},
    case maps:get(Key, Progress, -1) of
        #{score := PrevScore} when PrevScore =< Score ->
            filter_and_progress(Rest, Progress#{Key => State}, [State|Filtered]);
        _ ->
            filter_and_progress(Rest, Progress, Filtered)
    end.

get_neighbors(#{pos := Pos}, Map) ->
    {_, Exits} = maps:get(Pos, Map),
    Exits.

move(#{time := T, flow := Flow, sum := Sum, path := Path, since_last_open := SinceLast} = Current, To, Map) ->
    score_state(Current#{pos => To, time => T-1, sum => Sum + Flow, path => [{move, To}|Path], since_last_open => SinceLast#{To => true}}, Map).

open_valve(#{pos := Pos, time := T, flow := Flow, sum := Sum, path := Path, open := Open, not_open := Closed} = Current, Map) ->
    {V, _} = maps:get(Pos, Map),
    score_state(Current#{
        time => T-1,
        flow => Flow + V,
        sum => Sum + Flow,
        not_open => Closed - V,
        open => maps:put(Pos, true, Open),
        path => [{open, Pos}|Path],
        since_last_open => #{Pos => true}
    }, Map).

solve_2(Map) ->
    Map.
