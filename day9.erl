-module(day9).
-compile(export_all).

main(_) ->
    Input = read_input("9"),
    io:format("Input=~p~n", [Input]),
    Lows = find_lows(Input),
    io:format("Part1: ~p~n", [Lows]),
    ok.

next_atom(Atom) ->
    [N|_]=atom_to_list(Atom),
    list_to_atom([N+1]).

fill_in_lows(Map, [], _) ->
    Map;
fill_in_lows(Map, [{Key,_}|Lows], NextAtom) ->
    fill_in_lows(Map#{Key => NextAtom}, Lows, next_atom(NextAtom)).

fill_in_one(Map, Lows = [{{_,_},_}|_]) ->
    fill_in_one(Map, lists:map(fun ({K,_V}) -> K end, Lows));
fill_in_one(Map, []) ->
    Map;
fill_in_one(Map, [Low|Lows]) ->
    Basin = maps:get(Low, Map),
    Neighbors = neighbors(Map, Low),
    Neighbors1 = lists:filter(fun (K) -> is_integer(maps:get(K, Map)) end, Neighbors),
    Neighbors2 = lists:filter(fun (K) -> maps:get(K, Map) < 9 end, Neighbors1),
    Map1 = lists:foldl(fun (N, M) -> maps:put(N, Basin, M) end, Map, Neighbors2),
    fill_in_one(Map1, lists:append(Lows, Neighbors2)).

basin_score(Counts) ->
    TopThree = lists:sublist(lists:reverse(lists:sort(maps:values(Counts))), 3),
    lists:foldl(fun (Size,Product) -> Size*Product end, 1, TopThree).
    % TopThree.

basin_sizes(Map) ->
    basin_sizes(maps:to_list(Map), #{}).
basin_sizes([], Counts) ->
    Counts;
basin_sizes([{_,9}|Rest], Counts) ->
    basin_sizes(Rest, Counts);
basin_sizes([{_, Basin}|Rest], Counts) ->
    Counts1 = maps:update_with(Basin, fun (V) -> V+1 end, 1, Counts),
    basin_sizes(Rest, Counts1).

print(Map) ->
    print(Map, {0,0}).
print(Map, Coord = {R,C}) when is_map_key(Coord, Map) ->
    io:format("~p", [maps:get(Coord, Map)]),
    print(Map, {R, C+1});
print(Map, {R, 0}) when not is_map_key({R,0}, Map) ->
    done;
print(Map, {R, _C}) ->
    io:format("~n",[]),
    print(Map, {R+1, 0}).

risk_levels(Lows) ->
    lists:foldl(fun ({_, Height}, Sum) -> Sum + 1 + Height end, 0, Lows).

find_lows(Input) ->
    find_lows(Input, maps:to_list(Input), []).
find_lows(_, [], Lows) ->
    Lows;
find_lows(Map, [{{Row,Col},Val}|Search], Lows) ->
    Neighbors = neighbor_values(Map, {Row,Col}),
    % io:format("Neighbors of {~p,~p}->~p: ~p~n", [Row, Col, Val, Neighbors]),
    case lists:filter(fun (N) -> N =< Val end, Neighbors) of
        [] -> 
            % io:format("~p is a low point~n", [{Row,Col}]),
            find_lows(Map, Search, [{{Row,Col},Val}|Lows]);
        _ -> 
            find_lows(Map, Search, Lows)
    end.

neighbors(Map, {Row,Col}) ->
    neighbors(Map, Row, Col).
neighbors(Map, Row, Col) ->
    PossibleKeys = [
        {Row-1,Col},
        {Row+1,Col},
        {Row,Col-1},
        {Row,Col+1}
    ],
    lists:filter(fun (K) -> maps:is_key(K, Map) end, PossibleKeys).

neighbor_values(Map, {Row,Col}) ->
    lists:map(fun (K) -> maps:get(K, Map) end, neighbors(Map, Row, Col)).

read_input(Name) ->
    {ok, Data} = file:read_file(Name),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    parse_lines(Lines, 0, #{}).

parse_lines([], _, Result) ->
    Result;
parse_lines([Line|Lines], LineNumber, Result) ->
    Cols = re:split(Line, "", [trim]),
    {_, Result1} = lists:foldl(
        fun (Val, {ColNum, Res}) -> 
            Val1 = binary_to_integer(Val),
            {ColNum+1, maps:put({LineNumber, ColNum}, Val1, Res)} 
        end, {0, Result}, Cols),
    parse_lines(Lines, LineNumber+1, Result1).