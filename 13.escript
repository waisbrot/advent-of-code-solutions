#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname aoc -mnesia debug verbose
-mode(compile).

main(_) ->
    {Map,Folds} = read_input("13"),
    io:format("Map=~p~n", [Map]),
    print(Map),
    {FirstFoldPoints,_,_} = FirstFold = fold_map(Map, hd(Folds)),
    % io:format("After one fold:~n"), print(FirstFold),
    % io:format("~p~n", [FirstFold]),
    io:format("Dots after one fold: ~p~n", [maps:size(FirstFoldPoints)]),

    Folded = lists:foldl(fun (Fold, Map) -> fold_map(Map, Fold) end, Map, Folds),
    io:format("Folded~n"), print(Folded),
    % SecondFold = fold_map(FirstFold, lists:nth(2,Folds)),
    % io:format("After two folds:~n"), print(SecondFold),
    ok.

fold_map({Points,MaxX,MaxY}, {y,Cut}) ->
    CutLine = maps:filter(fun ({_,Y},_) -> Y =:= Cut end, Points),
    0 = maps:size(CutLine),
    Top = maps:filter(fun ({_,Y},_) -> Y < Cut end, Points),
    Bot = maps:filter(fun ({_,Y},_) -> Y > Cut end, Points),
    Bot1   = maps:from_keys(lists:map(fun ({X,Y}) -> {X,MaxY - Y} end, maps:keys(Bot  )), true),
    NewMap = maps:merge(Top, Bot1),
    MaxY1 = max_after_fold(MaxY, Cut),
    io:format("Real=~p Computed=~p~n", [find_min_max(NewMap), {MaxX, MaxY1}]),
    {NewMap, MaxX, MaxY1};
fold_map({Points,MaxX,MaxY}, {x,Cut}) ->
    CutLine = maps:filter(fun ({X,_},_) -> X =:= Cut end, Points),
    0 = maps:size(CutLine),
    Left  = maps:filter(fun ({X,_},_) -> X < Cut end, Points),
    Right = maps:filter(fun ({X,_},_) -> X > Cut end, Points),
    Right1 = maps:from_keys(lists:map(fun ({X,Y}) -> {MaxX - X,Y} end, maps:keys(Right)), true),
    NewMap = maps:merge(Left, Right1),
    MaxX1 = max_after_fold(MaxX, Cut),
    io:format("Real=~p Computed=~p~n", [find_min_max(NewMap), {MaxX1, MaxY}]),
    {NewMap, MaxX1, MaxY}.

max_after_fold(Max, Cut) ->
    Cut - 1.

find_min_max({Points,_,_}) ->
    find_min_max(Points);
find_min_max(Points) when is_map(Points) ->
    maps:fold(fun ({X,Y},_,{MaxX,MaxY}) -> {max(MaxX,X), max(MaxY,Y)} end, {0,0}, Points).

read_input(Name) ->
    {ok, Data} = file:read_file(Name),
    [MapBin,FoldsBin] = binary:split(Data, <<"\n\n">>, [trim]),
    Map = parse_map(MapBin),
    Folds = parse_folds(FoldsBin),
    {Map, Folds}.

parse_map(Data) ->
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    lists:foldl(
        fun (Line, {Map, MaxX, MaxY}) ->
            [X,Y] = lists:map(fun erlang:binary_to_integer/1, binary:split(Line, <<",">>)),
            Map1 = maps:put({X,Y},true, Map),
            MaxX1 = max(MaxX, X),
            MaxY1 = max(MaxY, Y),
            {Map1, MaxX1, MaxY1}
        end,
        {#{}, 0, 0},
        Lines).

parse_folds(Data) ->
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    lists:map(
        fun (Line) -> 
            <<"fold along ", FoldBin/binary>> = Line,
            [Axis, N] = binary:split(FoldBin, <<"=">>),
            {binary_to_atom(Axis), binary_to_integer(N)}
        end,
        Lines).

print(Map) ->
    print(Map, {0,0}).
print({_,_,MaxY}, {_,Y}) when Y > MaxY ->
    done;
print({_,MaxX,_} = Map, {X,Y}) when X > MaxX ->
    io:format("~n"),
    print(Map, {0, Y+1});
print({Points,_,_}=Map, {X,Y}=Coord) when is_map_key(Coord, Points) ->
    io:format("#"),
    print(Map, {X+1,Y});
print({Points,_,_}=Map, {X,Y}=Coord) when not is_map_key(Coord, Points) ->
    io:format("."),
    print(Map, {X+1,Y}).
