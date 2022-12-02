-module(aoc).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    solver(Args),
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
