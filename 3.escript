#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname aoc -mnesia debug verbose
-mode(compile).

read_file(Name) ->
    {ok, Data} = file:read_file(Name),
    re:split(Data, "\n", [trim]).

fill(Size, Fill) ->
    fill([], Size, Fill).

fill(List, 0, _Fill) ->
    List;
fill(List, Size, Fill) ->
    fill([Fill|List], Size - 1, Fill).

process1(Lines = [Head|_]) ->
    OneCounts = fill(size(Head), 0),
    process1(Lines, OneCounts, 0).
process1([], OneCounts, Total) ->
    [OneCounts, Total];
process1([Head|Rest], OneCounts, Total) ->
    NewOneCounts = update_counts(Head, OneCounts),
    NewTotal = Total + 1,
    process1(Rest, NewOneCounts, NewTotal).

summarize(OneCounts, Total) ->
    summarize(OneCounts, Total, []).
summarize([], _Total, RNumber) ->
    Number = lists:reverse(RNumber),
    Gamma = Number,
    Epsilon = compliment(Number),
    [list_to_integer(Gamma,2), list_to_integer(Epsilon,2)];
summarize([Head|Rest], Total, RNumber) when Head > Total/2 ->
    summarize(Rest, Total, [48|RNumber]);
summarize([Head|Rest], Total, RNumber) when Head < Total/2 ->
    summarize(Rest, Total, [49|RNumber]).

compliment(Number) ->
    compliment(lists:reverse(Number), []).
compliment([], Comp) ->
    Comp;
compliment([48|Rest], Comp) ->
    compliment(Rest, [49|Comp]);
compliment([49|Rest], Comp) ->
    compliment(Rest, [48|Comp]).

update_counts(String, Counts) ->
    update_counts(re:split(String, "", [trim]), Counts, []).
update_counts([], [], RCounts) ->
    lists:reverse(RCounts);
update_counts([<<"1">>|Rest], [Count|Counts], RCounts) ->
    update_counts(Rest, Counts, [Count+1|RCounts]);
update_counts([<<"0">>|Rest], [Count|Counts], RCounts) ->
    update_counts(Rest, Counts, [Count|RCounts]).

main(_) ->
    Lines = read_file("3"),
    [OneCounts, Total] = process1(Lines),
    FinalState1 = summarize(OneCounts, Total),
    io:format("1: ~p~n", [FinalState1]),

    [O2, CO2] = process2(Lines),
    io:format("2: O2: ~p; CO2: ~p~n", [binary_to_integer(O2, 2), binary_to_integer(CO2, 2)]).

process2(Lines) ->
    LineSize = length(re:split(hd(Lines), "", [trim])),
    [
        filter_lines(LineSize, Lines, 1, [], [], 0, most),
        filter_lines(LineSize, Lines, 1, [], [], 0, least)
    ].

filter_lines(_LineSize, [], _N, _Zeros, [One], Sign, most) when Sign >= 0 ->
    One;
filter_lines(_LineSize, [], _N, [Zero], _Ones, Sign, most) when Sign < 0 ->
    Zero;
filter_lines(_LineSize, [], _N, _Zeros, [One], Sign, least) when Sign < 0 ->
    One;
filter_lines(_LineSize, [], _N, [Zero], _Ones, Sign, least) when Sign >= 0 ->
    Zero;
filter_lines(LineSize, [], N, Zeros, _Ones, Sign, most) when Sign < 0 ->
    filter_lines(LineSize, Zeros, N+1, [], [], 0, most);
filter_lines(LineSize, [], N, _Zeros, Ones, Sign, most) when Sign >= 0 ->
    filter_lines(LineSize, Ones, N+1, [], [], 0, most);
filter_lines(LineSize, [], N, _Zeros, Ones, Sign, least) when Sign < 0 ->
    filter_lines(LineSize, Ones, N+1, [], [], 0, least);
filter_lines(LineSize, [], N, Zeros, _Ones, Sign, least) when Sign >= 0 ->
    filter_lines(LineSize, Zeros, N+1, [], [], 0, least);
filter_lines(LineSize, [Line|Lines], N, Zeros, Ones, Sign, Target) ->
    case lists:nth(N, re:split(Line, "", [trim])) of
        <<"0">> ->
            filter_lines(LineSize, Lines, N, [Line|Zeros], Ones, Sign - 1, Target);
        <<"1">> ->
            filter_lines(LineSize, Lines, N, Zeros, [Line|Ones], Sign + 1, Target)
    end.
