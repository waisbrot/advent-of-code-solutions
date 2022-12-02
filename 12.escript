#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname aoc -mnesia debug verbose
-mode(compile).

main(_) ->
    Map = read_input("12"),
    io:format("Map=~p~n", [Map]),
    Path = all_paths(Map),
    io:format("Paths=~p~nPath count=~p~n", [Path, length(Path)]),
    ok.

all_paths(Map) ->
    all_paths(Map, {small,start}, [], #{}, false, []).
all_paths(_Map, {small,'end'}, CurrentPath, _Visited, _DoubleSmall, SuccessfulPaths) ->
    ThisPath = lists:reverse(['end'|CurrentPath]),
    [ThisPath|SuccessfulPaths];
all_paths(_Map, Node, _CurrentPath, Visited, true, SuccessfulPaths) when is_map_key(Node, Visited) ->
    SuccessfulPaths; % backtrack
all_paths(Map, {big,_} = Node, CurrentPath, Visited, DoubleSmall, SuccessfulPaths) ->
    all_paths1(Map, Node, CurrentPath, Visited, DoubleSmall, SuccessfulPaths);
all_paths(Map, {small,_} = Node, CurrentPath, Visited, false, SuccessfulPaths) when is_map_key(Node, Visited) ->
    all_paths1(Map, Node, CurrentPath, Visited, true, SuccessfulPaths);
all_paths(Map, {small,_} = Node, CurrentPath, Visited, DoubleSmall, SuccessfulPaths) ->
    Visited1 = maps:put(Node, true, Visited),
    all_paths1(Map, Node, CurrentPath, Visited1, DoubleSmall, SuccessfulPaths).

all_paths1(Map, {_,CaveName} = Node, CurrentPath, Visited, DoubleSmall, SuccessfulPaths) ->
    CurrentPath1 = [CaveName|CurrentPath],
    NextPaths = lists:filter(fun ({small,start}) -> false; (_) -> true end, maps:get(Node, Map)),
    lists:foldl(
        fun (Cave, SuccessfulPathsAcc) ->
            all_paths(Map, Cave, CurrentPath1, Visited, DoubleSmall, SuccessfulPathsAcc)
        end,
        SuccessfulPaths, 
        NextPaths
    ).

read_input(Name) ->
    {ok, Data} = file:read_file(Name),
    Lines = re:split(Data, "\n", [trim]),
    parse_lines(Lines).

parse_lines(Lines) ->
    lists:foldl(
        fun (Line, Map) ->
            [Cave1, Cave2] = lists:map(fun parse_cave/1, binary:split(Line, <<"-">>)),
            Map1 = maps:update_with(Cave1, fun (V) -> [Cave2|V] end, [Cave2], Map),
            maps:update_with(Cave2, fun (V) -> [Cave1|V] end, [Cave1], Map1)
        end,
        #{},
        Lines).

parse_cave(Name) ->
    Size = case binary:first(Name) of
        Cap when Cap >= $A andalso Cap =< $Z ->
            big;
        _ ->
            small
    end,
    {Size, binary_to_atom(Name)}.