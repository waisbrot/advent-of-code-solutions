#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname aoc -mnesia debug verbose
-mode(compile).

main(_) ->
    {TargetX,TargetY} = Target = read_input("17"),
    io:format("Input=~p~n", [Target]),
    seek_max_y(0, -1000, TargetY),
    ok.

seek_max_y(Vel, Max, TargetY) ->
    NewMax = case throw_y(Vel, TargetY) of
        {hit, _, Best, _} when Best > Max ->
            io:format("New best. Velocity=~p MaxY=~p~n", [Vel, Best]),
            Best;
        _ when Vel rem 100 =:= 0 ->
            io:format("Starting velocity=~p~n", [Vel]),
            Max;
        1 ->
            Max
    end,
    seek_max_y(Vel+1, NewMax, TargetY).

throw_x(VelocityX, TargetX) ->
    throw_x(0, 0, 0, Velocity)

throw_y(VelocityY, {_,_}=TargetY) ->
    throw_y(0, 0, 0, VelocityY, TargetY).
throw_y(CurrentY, MaxSeen, Steps, VelocityY, {MinY, MaxY}) when CurrentY >= MinY, CurrentY =< MaxY ->
    {hit, CurrentY, MaxSeen, VelocityY, Steps};
throw_y(CurrentY, MaxSeen, Steps, _VelocityY, {MinY, _}) when CurrentY < MinY ->
    {miss, CurrentY, MaxSeen, Steps};
throw_y(CurrentY, MaxSeen, Steps, VelocityY, Target) ->
    MaxSeen1 = max(CurrentY, MaxSeen),
    Steps1 = Steps + 1,
    CurrentY1 = CurrentY + VelocityY,
    VelocityY1 = VelocityY - 1,
    throw_y(CurrentY1, MaxSeen1, Steps1, VelocityY1, Target).

read_input(Name) ->
    {ok, Data} = file:read_file(Name),
    [_, Areas] = binary:split(Data, <<": ">>),
    [AreaXB, AreaYB] = binary:split(Areas, <<", ">>),
    {parse_area_dim(AreaXB), parse_area_dim(AreaYB)}.

parse_area_dim(Bin) ->
    [_,Range] = binary:split(Bin, <<"=">>),
    [Min, Max] = binary:split(Range, <<"..">>),
    {binary_to_integer(Min), binary_to_integer(Max)}.
