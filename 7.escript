#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname aoc -mnesia debug verbose
-mode(compile).

main(_) ->
    Input = read_input("7"),
    io:format("Input=~p~n", [Input]),
    Median = median(Input),
    MedianCost = fuel_to_target(Input, Median),
    io:format("Median=~p Fuel=~p~n", [Median, MedianCost]),
    Max = lists:max(Input) + 1,
    Best = {
        walk_out(Input, Median-1, -1, -1, {Median, MedianCost}),
        walk_out(Input, Median+1, +1, Max, {Median, MedianCost})
    },
    io:format("Best=~p~n", [Best]),
    ok.

walk_out(_Numbers, Target, _Change, Limit, Best) when Target =:= Limit ->
    Best;
walk_out(Numbers, Target, Change, Limit, PrevBest) ->
    Cost = fuel_to_target(Numbers, Target),
    io:format("Target=~p Cost=~p~n", [Target, Cost]),
    Best = case PrevBest of
        {_, PrevCost} when PrevCost > Cost -> {Target, Cost};
        _ -> PrevBest
    end,
    walk_out(Numbers, Target + Change, Change, Limit, Best).

fuel_to_target(Numbers, Target) ->
    lists:foldl(
        fun (Num, Acc) ->
            Acc + fuel_cost(erlang:abs(Num - Target))
        end,
        0,
        Numbers).

fuel_cost(Distance) ->
    lists:sum(lists:seq(1, Distance)).

median(Numbers) ->
    Sorted = lists:sort(Numbers),
    Size = length(Sorted),
    case Size rem 2 of
        0 -> 
            Half = erlang:round(Size / 2),
            case {lists:nth(Half, Sorted), lists:nth(Half+1, Sorted)} of
                {Same, Same} -> Same;
                {A, B} -> (A+B)/2
            end;
        _ -> 
            lists:nth(erlang:round(Size/2), Sorted)
    end.

read_input(Name) ->
    {ok, Data} = file:read_file(Name),
    lists:map(fun erlang:binary_to_integer/1, re:split(Data, ",", [trim])).