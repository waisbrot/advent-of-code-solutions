-module(aoc_problem_3).
-export([solve/2]).

solve(Problem, Example) ->
    PacksRaw = aoc_input:split_by_lines(Problem, Example),
    Packs = lists:map(fun parse_pack/1, PacksRaw),
    %io:format("Packs=~p~n", [Packs]),
    Intersections = lists:map(fun find_common_item/1, Packs),
    io:format("Intersections=~p~n", [Intersections]),
    Scores = lists:map(fun score_item/1, Intersections),
    io:format("Scores=~p~n", [Scores]),
    io:format("Part 1: ~p~n", [lists:sum(Scores)]),
    io:format("Part 2: ~p~n", [part_2(PacksRaw)]).

parse_pack(PackString) ->
    Contents = binary_to_list(PackString),
    {A, B} = lists:split(length(Contents) div 2, Contents),
    {sets:from_list(A), sets:from_list(B)}.

find_common_item({A, B}) ->
    [Item] = sets:to_list(sets:intersection(A,B)),
    Item.

score_item(Item) when Item >= $A andalso Item =< $Z ->
    Item - 65 + 27;
score_item(Item) when Item >= $a andalso Item =< $z ->
    Item - 97 + 1.

part_2(Packs) ->
    part_2(lists:map(fun erlang:binary_to_list/1, Packs), 0).

part_2([], Sum) ->
    Sum;
part_2([A, B, C | Rest], Sum) ->
    [Item] = intersect_all([A, B, C]),
    Score = score_item(Item),
    part_2(Rest, Sum + Score).

intersect_all([Head|_] = Lists) ->
    sets:to_list(lists:foldl(fun intersect_folder/2, sets:from_list(Head), Lists)).
intersect_folder(List, Set) ->
    sets:intersection(sets:from_list(List), Set).