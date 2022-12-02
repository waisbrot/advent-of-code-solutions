#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname aoc -mnesia debug verbose
-mode(compile).

main(_) ->
    #{lines := Lines} = Input = read_input("5"),
    io:format("Input: ~p~n", [Input]),
    FilteredLines = filter_diagonals(Lines),
    io:format("Filtered: ~p~n", [FilteredLines]),
    ets:new(points, [named_table]),
    draw_lines(FilteredLines),
    OverOne1 = count_points(),
    io:format("Part 1: ~p~n", [OverOne1]),
    
    ets:delete_all_objects(points),
    draw_lines(Lines),
    OverOne2 = count_points(),
    io:format("Part 2: ~p~n", [OverOne2]),
    ok.

count_points() ->
    lists:foldl(fun ({{_, _}, N}, C) ->
        if N > 1 -> C+1; true -> C end
        end,
        0,
        ets:tab2list(points)
    ).

draw_lines(Points) ->
    lists:foreach(fun draw_line/1, Points).

draw_line({{X, Y}, {X, Y}}) ->
    ets:update_counter(points, {X,Y}, 1, {{X, Y}, 0});
draw_line({{X, Y1}, {X, Y2}}) ->
    ets:update_counter(points, {X,Y1}, 1, {{X, Y1}, 0}),
    draw_line({{X, Y1+1}, {X, Y2}});
draw_line({{X1, Y}, {X2, Y}}) ->
    ets:update_counter(points, {X1,Y}, 1, {{X1, Y}, 0}),
    draw_line({{X1+1, Y}, {X2, Y}});
draw_line({{X1, Y1}, {X2, Y2}}) when X1 < X2, Y1 < Y2 ->
    ets:update_counter(points, {X1,Y1}, 1, {{X1, Y1}, 0}),
    draw_line({{X1+1, Y1+1}, {X2, Y2}});
draw_line({{X1, Y1}, {X2, Y2}}) when X1 < X2, Y1 > Y2 ->
    ets:update_counter(points, {X1,Y1}, 1, {{X1, Y1}, 0}),
    draw_line({{X1+1, Y1-1}, {X2, Y2}}).


filter_diagonals(Lines) ->
    filter_diagonals(Lines, []).
filter_diagonals([], Filtered) ->
    Filtered;
filter_diagonals([{{X1,Y1},{X2,Y2}} = Point|Lines], Filtered) when X1 =:= X2; Y1 =:= Y2 ->
    filter_diagonals(Lines, [Point|Filtered]);
filter_diagonals([_|Lines], Filtered) ->
    filter_diagonals(Lines, Filtered).

read_input(Name) ->
    {ok, Data} = file:read_file(Name),
    LinesStr = re:split(Data, "\n", [trim]),
    parse_lines(LinesStr, [], 0).

parse_lines([], Parsed, MaxDim) ->
    #{lines => Parsed, max_dim => MaxDim};
parse_lines([Line|Lines], Parsed, OldMax) ->
    [From,To] = re:split(Line, " -> ", [trim]),
    [X1b,Y1b] = re:split(From, ","),
    [X2b,Y2b] = re:split(To, ","),
    [X1,Y1,X2,Y2] = Dims = [binary_to_integer(N) || N <- [X1b, Y1b, X2b, Y2b]],
    Max = lists:max([OldMax|Dims]),
    Points = if
        X2 < X1 -> {{X2, Y2}, {X1, Y1}};
        X1 < X2 -> {{X1, Y1}, {X2, Y2}};
        Y2 < Y1 -> {{X2, Y2}, {X1, Y1}};
        true -> {{X1, Y1}, {X2, Y2}}
    end,
    parse_lines(Lines, [Points|Parsed], Max).
