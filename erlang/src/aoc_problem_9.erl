-module(aoc_problem_9).
-export([solve/2]).

solve(Problem, Example) ->
    Directions = aoc_timer:input(fun ingest/2, [Problem, Example]),
    aoc_timer:problem(1, fun solve_1/1, [Directions]),
    aoc_timer:problem(2, fun solve_2/1, [Directions]).

ingest(Problem, Example) ->
    Lines = aoc_input:split_by_lines(Problem, Example),
    lists:flatmap(fun parse_expand_line/1, Lines).

parse_expand_line(<<Dir:1/bytes, " ", Dist/binary>>) -> 
    Direction = binary_to_atom(string:lowercase(Dir)),
    Distance = binary_to_integer(Dist),
    lists:duplicate(Distance, Direction).

solve_1(Directions) ->
    Head = {0,0},
    Tail = {0,0},
    Visited = travel(Directions, Head, Tail, sets:new()),
    sets:size(Visited).

travel([], _, _, Visited) ->
    Visited;
travel([Direction|Directions], Head, Tail, Visited) ->
    % io:format("Travel ~p: Head=~p Tail=~p~n", [Direction, Head, Tail]),
    Head2 = move_head(Head, Direction),
    Tail2 = move_tail(Head2, Tail),
    Visited2 = sets:add_element(Tail2, Visited),
    travel(Directions, Head2, Tail2, Visited2).

move_head({X, Y}, r) -> {X+1, Y};
move_head({X, Y}, l) -> {X-1, Y};
move_head({X, Y}, u) -> {X, Y+1};
move_head({X, Y}, d) -> {X, Y-1}.

move_tail({Xh, Yh}, {Xt, Yt} = Tail) ->
    Dx = Xh - Xt,
    Dy = Yh - Yt,
    % io:format("Move tail: Head={~p,~p} Tail=~p Dx=~p Dy=~p~n", [Xh, Yh, Tail, Dx, Dy]),
    if 
        abs(Dx) =< 1 andalso abs(Dy) =< 1 ->
            % io:format("No movement~n"),
            Tail;
        abs(Dx) =:= 2 ->
            Xt2 = Xt + (Dx div 2),
            Yt2 = if
                Dy =:= 0 -> Yt;
                abs(Dy) =:= 2 -> Yt + (Dy div 2);
                true -> Yt+Dy
            end,
            % io:format("New tail from X pull: ~p~n", [{Xt2, Yt2}]),
            {Xt2, Yt2};
        abs(Dy) =:= 2 ->
            Yt2 = Yt + (Dy div 2),
            Xt2 = if 
                Dx =:= 0 -> Xt;
                true -> Xt+Dx
            end,
            % io:format("New tail from Y pull: ~p~n", [{Xt2, Yt2}]),
            {Xt2, Yt2}
    end.

solve_2(Directions) ->
    Rope = lists:duplicate(10, {0,0}),
    Visited = travel_rope(Directions, Rope, sets:new()),
    sets:size(Visited).

travel_rope([], _, Visited) ->
    Visited;
travel_rope([Direction|Directions], [Head|Rope], Visited) ->
    Head2 = move_head(Head, Direction),
    % io:format("## Rope head moved ~p -> ~p~n", [Head, Head2]),
    {Rope2, Tail} = lists:mapfoldl(fun (T, H) -> T2 = move_tail(H, T), {T2, T2} end, Head2, Rope),
    Visited2 = sets:add_element(Tail, Visited),
    travel_rope(Directions, [Head2|Rope2], Visited2).
