-module(aoc_problem_15).
-export([solve/2]).

solve(Problem, Example) ->
    {Sensors, TargetY} = aoc_timer:input(fun ingest/2, [Problem, Example]),
    aoc_timer:problem(1, fun solve_1/2, [Sensors, TargetY]),
    aoc_timer:problem(2, fun solve_2/1, [Sensors]).

ingest(Problem, Example) ->
    Lines = aoc_input:split_by_lines(Problem, Example),
    {ok, LineEx} = re:compile("Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)", [anchored]),
    TargetY = if Example =:= e -> 10; true -> 2000000 end,
    Sensors = lists:map(fun (Line) -> 
        {match, M} = re:run(Line, LineEx, [{capture, all_but_first, binary}]),
        M2 = lists:map(fun erlang:binary_to_integer/1, M),
        {A,B} = lists:split(2, M2),
        #{sensor => list_to_tuple(A), beacon => list_to_tuple(B)}
    end, Lines),
    {Sensors, TargetY}.

solve_1(Sensors, TargetY) ->
    % io:format("~p sensors~n", [length(Sensors)]),
    Sensors2 = lists:map(fun add_range/1, Sensors),
    Sensors3 = lists:filter(fun (Sensor) -> target_within_range(Sensor, TargetY) end, Sensors2),
    % io:format("~p in range of y=~p~n", [length(Sensors3), TargetY]),
    TargetRow = lists:foldl(fun (Sensor, Visible) ->
        Segment = all_x_in_range(Sensor, TargetY),
        join_segment(Visible, Segment)
    end, [], Sensors3),
    Beacons = lists:map(fun (#{beacon := B}) -> B end, Sensors),
    Beacons2 = lists:filter(fun ({_,Y}) -> Y =:= TargetY end, Beacons),
    Beacons3 = sets:from_list(lists:map(fun ({X,_}) -> X end, Beacons2)),
    io:format("All X in range: ~p~n", [TargetRow]),
    io:format("Less beacons ~p~n", [Beacons3]),
    points_in_segments(TargetRow) - sets:size(Beacons3).

add_range(#{sensor := Sen, beacon := Bea} = P) ->
    P#{range => manattan_distance(Sen, Bea)}.

% copy/paste from day 12 
manattan_distance({R1, C1}, {R2, C2}) ->
    abs(R1 - R2) + abs(C1 - C2).

target_within_range(#{sensor := {_,Y}, range := R}, Ty) ->
    % io:format("target_within_range y=~p r=~p t=~p -> ~p~n", [Y, R, Ty, abs(Y - Ty) =< R]),
    abs(Y - Ty) =< R.

all_x_in_range(#{sensor := {X,Y}, range := R}, Ty) ->
    Wiggle = R - abs(Y - Ty),
    {X-Wiggle, X+Wiggle}.

join_segment(Segments, New) ->
    Res = lists:sort(lists:flatten(join_segment(Segments, New, []))),
    io:format("Seg=~p New=~p -> ~p~n", [Segments, New, Res]),
    Res.

join_segment([], New, Joint) ->
    [New|Joint];
join_segment([{L0,_}|_]=R, {_,H}=New, Joint) when H < L0 ->
    [New,R|Joint];
join_segment([{_,H0}=S|R], {L,_}=New, Joint) when L > H0 ->
    join_segment(R, New, [S|Joint]);
join_segment([{L0,H0}|R], {L,H}, Joint) ->
    join_segment(R, {min(L0,L), max(H0,H)}, Joint).

join_segments(S1, S2) ->
    join_segments(S1, S2, []).

join_segments([], [], J) -> lists:reverse(J);
join_segments(S1, [], J) -> lists:append(lists:reverse(J), S1);
join_segments([], S2, J) -> lists:append(lists:reverse(J), S2);
join_segments([{L1,H1}|R1], [{L2,H2}|R2], J) when (L2 >= L1 andalso L2 =< H1); (L1 >= L2 andalso L1 =< H2); (H1 >= L2 andalso H1 =< H2); (H2 >= L1 andalso H2 =< H1) -> 
    % overlap
    Combined = {min(L1,L2), max(H1,H2)},
    join_segments(R1,[Combined|R2], J);
join_segments([{L1,_H1}=S1|R1], [{L2,_H2}|_]=S2, J) when L1 < L2 ->
    join_segments(R1, S2, [S1|J]);
join_segments([{L1,_H1}=S1|_], [{L2,_H2}=S2|R2], J) when L2 < L1 ->
    join_segments(S1, R2, [S2|J]).

points_in_segments(Segments) ->
    lists:foldl(fun ({L, H}, S) -> S + (H - L) + 1 end, 0, Segments).

solve_2(Sensors) ->
    length(Sensors).