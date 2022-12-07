-module(aoc_problem_6).
-export([solve/2]).

solve(Problem, Example) ->
    {TimeInput, Binary} = timer:tc(fun aoc_input:simple_read/2, [Problem, Example]),
    io:format("Read input: ~p seconds~n", [TimeInput * 10.0e-7]),
    {TimeFirst, Position} = timer:tc(fun first_unique_n/2, [4, Binary]),
    io:format("Problem 1: ~p in ~p seconds~n", [Position, TimeFirst * 10.0e-7]),
    {TimeMessage, Position2} = timer:tc(fun first_unique_n/2, [14, Binary]),
    io:format("Problem 2: ~p in ~p seconds~n", [Position2, TimeMessage * 10.0e-7]).

first_unique_n(N, <<Head:1/bytes, Binary/binary>>) ->
    search(N, 0, #{}, queue:new(), Head, Binary).

search(Target, Position, Set, _Queue, _Head, _Binary) when map_size(Set) =:= Target ->
    Position;
search(Target, Position, Set, Queue, Head, Binary) when is_map_key(Head, Set) ->
    %io:format("Duplicate Pos=~p, Set=~p Queue=~p Head=~p~n", [Position, Set, Queue, Head]),
    {{value, Out}, Queue2} = queue:out(Queue),
    Set2 = maps:remove(Out, Set),
    %io:format("Remove ~p from ~p = ~p~n", [Out, Set, Set2]),
    search(Target, Position, Set2, Queue2, Head, Binary);
search(Target, Position, Set, Queue, Head, <<Head2:1/bytes, Binary/binary>>) ->
    %io:format("Non-duplicate Pos=~p, Set=~p Head=~p~n", [Position, Set, Head]),
    search(Target, Position + 1, Set#{Head => 1}, queue:in(Head, Queue), Head2, Binary).
