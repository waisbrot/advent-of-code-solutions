#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname aoc -mnesia debug verbose
-mode(compile).

main(_) ->
    {Start,Map} = read_input("14.example"),
    io:format("Input=~p~n", [{Start,Map}]),
    StartPairs = to_pair_counts(to_pairs(Start)),
    io:format("StartPairs=~p~n", [StartPairs]),
    Pass3 = nth_pass(3, StartPairs, Map),
    io:format("3 pass: ~p~n", [Pass3]),
    Pass4 = one_pass(Pass3, Map),
    io:format("4 pass: ~p~n", [Pass4]),
    % Part1 = part1_score(Pass10),
    % io:format("Max - Min = ~p~n", [Part1]),

    % Pass40 = nth_pass(40, StartPairs, Map),
    % Count40 = counts(Pass40),
    % Part2 = part1_score(Count40),
    % io:format("Part2: ~p~n", [Part2]),
    ok.

part1_score(Counts) ->
    V = maps:values(Counts),
    Max = lists:max(V),
    Min = lists:min(V),
    Max - Min.

nth_pass(0, Pairs, _Map) ->
    Pairs;
nth_pass(N, Pairs, Map) when N > 0 ->
    Pairs1 = one_pass(Pairs, Map),
    io:format("Pass=~p Pairs=~p~n", [N, Pairs1]),
    nth_pass(N-1, Pairs1, Map).

counts(Pairs = [{_,_}|_]) ->
    Elements = from_pairs(Pairs),
    lists:foldl(fun (El, Count) -> maps:update_with(El, fun (V) -> V+1 end, 1, Count) end, #{}, Elements).


to_pairs([H|T]) ->
    {_, Pairs} = lists:foldl(fun (El, {Prev,Pairs}) -> {El, [{Prev,El}|Pairs]} end, {H,[]}, T),
    lists:reverse(Pairs).

increment(N) -> N+1.
decrement(N) -> N-1.

to_pair_counts(Pairs) ->
    {_,B} = lists:last(Pairs),
    lists:foldl(
        fun ({A,B} = Pair, {PairCounts, ElementCounts}) ->
            PairCounts1 = maps:update_with(Pair, fun increment/1, 1, PairCounts),
            ElementCounts1 = maps:update_with(A, fun increment/1, 1, ElementCounts),
            {PairCounts1, ElementCounts1}
        end,
        {#{}, #{B => 1}},
        Pairs).

from_pairs(Pairs) ->
    lists:reverse(from_pairs(Pairs, [])).
from_pairs([{A,B}], Acc) ->
    [B,A|Acc];
from_pairs([{A,_}|Rest], Acc) ->
    from_pairs(Rest, [A|Acc]).

one_pass({PairCounts, ElementCounts},Map) -> 
    {PairCounts1, NewElements, _} = lists:foldl(fun one_pass_helper/2, {PairCounts, #{}, Map}, maps:to_list(PairCounts)),
    ElementCounts1 = maps:merge_with(fun (_K, V1, V2) -> V1 + V2 end, ElementCounts, NewElements),
    {PairCounts1, ElementCounts1}.

one_pass_helper({{A,B}=Pair, Count}, {PairCounts, NewElements, Map}) when Count > 0 ->
    PairCounts1 = maps:remove(Pair, PairCounts),
    NewElement = maps:get(Pair, Map),
    AddCount = fun (V) ->
        Sum = V + Count,
        io:format("Updating: ~p+~p=~p~n", [V,Count,Sum]),
        Sum
    end,
    PairCounts2 = maps:update_with({A, NewElement}, AddCount, Count, PairCounts1),
    PairCounts3 = maps:update_with({NewElement, B}, AddCount, Count, PairCounts2),
    NewElements1 = maps:update_with(NewElement, AddCount, Count, NewElements),
    {PairCounts3, NewElements1, Map};
one_pass_helper(_, Acc) ->
    Acc.

read_input(Name) ->
    {ok, Data} = file:read_file(Name),
    Data1 = string:lowercase(Data),
    [StartBin,MapBin] = binary:split(Data1, <<"\n\n">>, [trim]),
    Map = parse_map(MapBin),
    Start = parse_start(StartBin),
    {Start, Map}.

parse_start(Bin) ->
    Letters = re:split(Bin, "", [trim]),
    lists:map(fun erlang:binary_to_atom/1, Letters).

parse_map(Bin) ->
    Lines = binary:split(Bin, <<"\n">>, [global, trim_all]),
    lists:foldl(fun parse_map_line/2, #{}, Lines).

parse_map_line(Line, Map) ->
    [From, To] = binary:split(Line, <<" -> ">>),
    [FromA,FromB] = re:split(From, "", [trim]),
    [A,B,C] = lists:map(fun erlang:binary_to_atom/1, [FromA, FromB, To]),
    maps:put({A,B}, C, Map).
