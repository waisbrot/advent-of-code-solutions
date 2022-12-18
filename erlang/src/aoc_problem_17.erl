-module(aoc_problem_17).
-export([solve/2]).

solve(Problem, Example) ->
    Movements = aoc_timer:input(fun ingest/2, [Problem, Example]),
    aoc_timer:problem(1, fun solve_1/1, [Movements]),
    aoc_timer:problem(2, fun solve_2/1, [Movements]).

ingest(Problem, Example) ->
    Binary = aoc_input:simple_read(Problem, Example),
    lists:map(fun ($<) -> left; ($>) -> right end, binary_to_list(Binary)).

solve_1(Moves) ->
    {NextRock, Rocks} = rotation:next(rotation:from_list([horizontal, cross, jay, vertical, square])),
    Moves2 = rotation:from_list(Moves),
    Floor = lists:duplicate(7, 0),
    drop_rocks(NextRock, Rocks, Floor, 2022, Moves2).

solve_2(Moves) ->
    Moves.

drop_rocks(_, _, Floor, 0, _) ->
    lists:max(Floor);
drop_rocks(NextRock, Rocks, Floor, Remaining, Moves) ->
    print_floor(Floor),
    MaxFloor = lists:max(Floor),
    {Move, Moves2} = rotation:next(Moves),
    {Floor2, Moves3} = drop_rock(NextRock, push, Move, 3, MaxFloor + 4, Floor, Moves2),
    {NextRock2, Rocks2} = rotation:next(Rocks),
    drop_rocks(NextRock2, Rocks2, Floor2, Remaining-1, Moves3).

print_floor(Floor) ->
    lists:foreach(fun ({I,C}) -> io:format("~p: ~p~n", [I, lists:duplicate(C, $#)]) end, lists:enumerate(Floor)).

maybe_move(_, _, _, _) -> ok.

% Left
drop_rock(Type, push, left, 1, Up, Floor, Moves) ->
    io:format("Leftward ~p Left=1 Up=~p~n", [Type, Up]),
    drop_rock_next(Type, push, 1, Up, Floor, Moves);
drop_rock(Type, push, left, N, Up, Floor, Moves) ->
    io:format("Leftward ~p Left=~p Up=~p~n", [Type, N, Up]),
    drop_rock_next(Type, push, N-1, Up, Floor, Moves);
drop_rock(Type, push, right, Left, Up, Floor, Moves) ->
    Left2 = case 7 - width(Type) of
        N when N > Left ->
            maybe_move(Type, left, Left, Floor);
        _ -> 
            todo
        end;
% Falling
drop_rock(Type, drop, _, Left, Up, Floor, Moves) when Up > 0 ->
    io:format("Dropping ~p Left=~p Up=~p~n", [Type, Left, Up]),
    case resting_on_floor(Type, Left, Up, Floor) of
        {stopped, Floor2} ->
            {Floor2, Moves};
        falling ->
            drop_rock_next(Type, drop, Left, Up-1, Floor, Moves)
    end;
% Horizontal
drop_rock(horizontal, push, right, 4, Up, Floor, Moves) ->
    io:format("Rightward horizontal Left=4 Up=~p~n", [Up]),
    drop_rock_next(horizontal, push, 4, Up, Floor, Moves);
% Cross
drop_rock(cross, push, right, 5, Up, Floor, Moves) ->
    drop_rock_next(cross, push, 5, Up, Floor, Moves);
% Jay
drop_rock(jay, push, right, 5, Up, Floor, Moves) ->
    drop_rock_next(jay, push, 5, Up, Floor, Moves);
% Vertical
drop_rock(vertical, push, right, 7, Up, Floor, Moves) ->
    drop_rock_next(vertical, push, 7, Up, Floor, Moves);
% square
drop_rock(square, push, right, 6, Up, Floor, Moves) ->
    drop_rock_next(square, push, 6, Up, Floor, Moves);
% Default right-push if not caught earlier
drop_rock(Type, push, right, N, Up, Floor, Moves) ->
    io:format("Rightward ~p Left=~p Up=~p~n", [Type, N, Up]),
    drop_rock_next(Type, push, N+1, Up, Floor, Moves).

width(horizontal) -> 4;
width(cross) -> 3;
width(jay) -> 3;
width(vertical) -> 1;
width(square) -> 2.


resting_on_floor(horizontal, Left, Up, Floor) ->
    UnderBlock = lists:sublist(Floor, Left, 4),
    AtRest = lists:max(UnderBlock) =:= (Up - 1),
    if
        AtRest ->
            Floor2 = lists:append([lists:sublist(Floor, Left-1), lists:duplicate(4, Up), lists:nthtail(Left+4-1, Floor)]),
            {stopped, Floor2};
        true ->
            falling
    end;
resting_on_floor(cross, Left, Up, Floor) ->
    [U1,U2,U3] = lists:sublist(Floor, Left, 3),
    AtRest = (U1 =:= Up orelse U2+1 =:= Up orelse U3 =:= Up),
    if
        AtRest ->
            Floor2 = lists:append([lists:sublist(Floor, Left-1), [Up+1,Up+2,Up+1], lists:nthtail(Left+3-1, Floor)]),
            {stopped, Floor2};
        true ->
            falling
    end;
resting_on_floor(jay, Left, Up, Floor) ->
    UnderBlock = lists:sublist(Floor, Left, 3),
    AtRest = lists:max(UnderBlock) =:= (Up - 1),
    io:format("resting? UnderBlock=~p Left=~p Up=~p AtRest=~p~n", [UnderBlock, Left, Up, AtRest]),
    if
        AtRest ->
            Floor2 = lists:append([lists:sublist(Floor, Left-1), [Up, Up, Up+2], lists:nthtail(Left+3-1, Floor)]),
            {stopped, Floor2};
        true ->
            falling
    end.

% handle flopping between dropping and pushing
drop_rock_next(Type, push, Left, Up, Floor, Moves) ->
    drop_rock(Type, drop, down, Left, Up, Floor, Moves);
drop_rock_next(Type, drop, Left, Up, Floor, Moves) ->
    {Move, Moves2} = rotation:next(Moves),
    drop_rock(Type, push, Move, Left, Up, Floor, Moves2).