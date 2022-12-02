#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname aoc -mnesia debug verbose
-mode(compile).

main(_) ->
    Input = read_input("6"),
    io:format("Input: ~p~n", [Input]),
    Ages = n_days(Input, 256),
    ResultOne = sum_fish(Ages),
    io:format("1: ~p~n", [ResultOne]),
    ok.

sum_fish(Ages) ->
    maps:fold(fun (_K,V, Sum) -> Sum + V end, 0, Ages).

n_days(Ages, 0) ->
    Ages;
n_days(Ages, Days) ->
    case Days rem 10 of
        0 -> io:format("Days=~p; Ages=~p; Fish=~p~n", [Days,Ages, sum_fish(Ages)]);
        _ -> ok
    end,
    n_days(one_day(Ages), Days - 1).

one_day(#{0 := NewFish} = Ages) ->
    NewAges = #{6 := SixFish} = decrement_ages(Ages#{0 => 0}),
    NewAges1 = NewAges#{6 => SixFish + NewFish},
    NewAges2 = NewAges1#{8 => NewFish},
    NewAges2.

decrement_ages(Ages) ->
    decrement_ages(Ages, 1).
decrement_ages(Ages, 9) ->
    Ages;
decrement_ages(Ages, FromAge) ->
    #{FromAge := N} = Ages,
    ToAge = FromAge - 1,
    decrement_ages(Ages#{ToAge => N, FromAge := 0}, FromAge+1).

read_input(Name) ->
    {ok, Data} = file:read_file(Name),
    LinesBin = re:split(Data, ",", [trim]),
    LinesInt = lists:map(fun erlang:binary_to_integer/1, LinesBin),
    parse_input(LinesInt).

parse_input(Ages) ->
    Init = lists:foldl(fun (K, M) -> maps:put(K, 0, M) end, #{}, lists:seq(0,8)),
    lists:foldl(
            fun (Age, Map) ->
                maps:update_with(Age, fun (A) -> A+1 end, Map)
            end,
            Init,
            Ages
    ).