-module(aoc).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {Time, _} = timer:tc(fun solver/1, Args),
    io:format("Took ~p microseconds~n", [Time]),
    erlang:halt(0).

solver([Problem]) ->
    solver(Problem, false);
solver([Problem, "e"]) ->
    solver(Problem, true).

solver(Problem, Example) ->
    Module = list_to_atom(lists:flatten(["aoc_problem_", Problem])),
    Module:solve(Problem, Example).
    
%%====================================================================
%% Internal functions
%%====================================================================
