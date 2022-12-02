#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname aoc -mnesia debug verbose
-mode(compile).

main(_) ->
    Map = read_input("11"),
    print(Map),io:format("~n"),
    {_Map1, Flashes} = steps(100, Map, 0),
    io:format("Total flashes: ~p~n", [Flashes]),
    Steps = steps_to_full(Map),
    io:format("~p steps until full flash~n", [Steps]),
    ok.

steps_to_full(Map) ->
    TargetCount = map_size(Map),
    steps_to_full(Map, TargetCount, 1).
steps_to_full(Map, TargetCount, Steps) ->
    case step(Map) of
        {_, FlashCount} when FlashCount =:= TargetCount ->
            Steps;
        {Map1, _} ->
            steps_to_full(Map1, TargetCount, Steps+1)
    end.

steps(0, Map, Flashcount) ->
    {Map, Flashcount};
steps(N, Map, Flashcount) ->
    {Map1, Flashes} = step(Map),
    % io:format("Flashes=~p~n", [Flashes]),
    % print(Map1),
    % io:format("~n"),
    steps(N-1, Map1, Flashcount+Flashes).

step(Map) ->
    step_a(Map, [], {0,0}).
step_a(Map, Flashes, {_,0} = Next) when not is_map_key(Next, Map) ->
    % io:format("To step b with Flashes=~p~n", [Flashes]),
    step_b(Map, Flashes, #{});
step_a(Map, Flashes, {R,_} = Next) when not is_map_key(Next, Map) ->
    step_a(Map, Flashes, {R+1, 0});
step_a(Map, Flashes, {R,C} = Next) ->
    N = maps:get(Next, Map) + 1,
    Map1 = maps:put(Next, N, Map),
    Flashes1 = if N > 9 -> [Next|Flashes]; true -> Flashes end,
    step_a(Map1, Flashes1, {R,C+1}).

step_b(Map, [], Flashes) ->
    step_c(Map, maps:keys(Flashes), 0);
step_b(Map, [Oct|Queue], Flashes) when is_map_key(Oct, Flashes) ->
    step_b(Map, Queue, Flashes);
step_b(Map, [Oct|Queue], Flashes) ->
    Flashes1 = maps:put(Oct, true, Flashes),
    {Map1, AddOcts} = increment_neighbors(Map, Oct),
    step_b(Map1, lists:append(Queue, AddOcts), Flashes1).

step_c(Map, [], Count) ->
    {Map, Count};
step_c(Map, [Flashed|Rest], Count) ->
    Map1 = maps:put(Flashed, 0, Map),
    step_c(Map1, Rest, Count+1).

increment_neighbors(Map, Coord) ->
    Neighbors = neighbors(Map, Coord),
    increment_neighbors(Map, Neighbors, []).
increment_neighbors(Map, [], New) ->
    {Map, New};
increment_neighbors(Map, [N|Rest], New) when map_get(N, Map) =:= 9 ->
    Map1 = maps:put(N, 10, Map),
    increment_neighbors(Map1, Rest, [N|New]);
increment_neighbors(Map, [N|Rest], New) ->
    Map1 = maps:update_with(N, fun (V) -> V+1 end, Map),
    increment_neighbors(Map1, Rest, New).

neighbors(Map, {Row,Col}) ->
    neighbors(Map, Row, Col).
neighbors(Map, Row, Col) ->
    PossibleKeys = [
        {Row-1,Col}, % N
        {Row+1,Col}, % S
        {Row,Col-1}, % W
        {Row,Col+1}, % E
        {Row-1,Col-1}, % NW
        {Row-1,Col+1}, % NE
        {Row+1,Col-1}, % SW
        {Row+1,Col+1}  % SE
    ],
    lists:filter(fun (K) -> maps:is_key(K, Map) end, PossibleKeys).

print(Map) ->
    print(Map, {0,0}).
print(Map, Coord = {R,C}) when is_map_key(Coord, Map) ->
    io:format("~b", [maps:get(Coord, Map)]),
    print(Map, {R, C+1});
print(Map, {_, 0} = Coord) when not is_map_key(Coord, Map) ->
    done;
print(Map, {R, _}) ->
    io:format("~n"),
    print(Map, {R+1, 0}).

read_input(Name) ->
    {ok, Data} = file:read_file(Name),
    Lines = re:split(Data, "\n", [trim]),
    parse_lines(Lines, 0, #{}).

parse_lines([], _, Map) ->
    Map;
parse_lines([Line|Lines], Count, Map) ->
    Row = re:split(Line, "", [trim]),
    Map1 = parse_row(Row, Count, 0, Map),
    parse_lines(Lines, Count+1, Map1).

parse_row([], _, _, Map) ->
    Map;
parse_row([Col|Cols], RowNum, ColNum, Map) ->
    Map1 = maps:put({RowNum, ColNum}, binary_to_integer(Col), Map),
    parse_row(Cols, RowNum, ColNum+1, Map1).