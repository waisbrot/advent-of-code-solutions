#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname aoc -mnesia debug verbose
-mode(compile).
read_file(Name) ->
    {ok, Data} = file:read_file(Name),
    re:split(Data, "\n").

process(Lines) ->
    process(Lines, #{distance => 0, depth => 0, aim => 0}).

process([], State) ->
    State;
process([Direction|Rest], State) ->
    NewState = handle_direction(Direction, State),
    process(Rest, NewState).

handle_direction(<<>>, State) ->
    State;
handle_direction(Direction, State) ->
    [Name, Sizes] = re:split(Direction, " "),
    Size = binary_to_integer(Sizes),
    handle_direction(Name, Size, State).

handle_direction(<<"forward">>, Size, #{distance := Dist, depth := Dep, aim := Aim} = State) ->
    NewDist = Dist + Size,
    NewDep = Dep + (Size * Aim),
    io:format("Aim: ~p; Dist: ~p -> ~p; Dep: ~p -> ~p~n", [Aim, Dist, NewDist, Dep, NewDep]),
    State#{distance => NewDist, depth => NewDep };
handle_direction(<<"up">>, Size, #{aim := Aim} = State) ->
    NewAim = Aim - Size,
    io:format("Aim: ~p -> ~p~n", [Aim, NewAim]),
    State#{aim => NewAim};
handle_direction(<<"down">>, Size, #{aim := Aim} = State) ->
    NewAim = Aim + Size,
    io:format("Aim: ~p -> ~p~n", [Aim, NewAim]),
    State#{aim => NewAim}.

main(_) ->
    Lines = read_file("2"),
    FinalState = process(Lines),
    #{depth := Dep, distance := Dis} = FinalState,
    io:format("State: ~p~nCalc: ~p~n", [FinalState, Dep * Dis]).
