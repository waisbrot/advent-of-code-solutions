-module(aoc_problem_2).
-export([solve/2]).

solve(Problem, Example) ->
    RPSStrs = aoc_input:split_by_lines(Problem, Example),
    RPS = lists:map(fun parse_rps_line/1, RPSStrs),
    %io:format("RPS=~p~n", [RPS]),
    Scores = lists:map(fun rps_score/1, RPS),
    Total = lists:sum(Scores),
    io:format("Part 1: ~p~n", [Total]),
    RPS2 = lists:map(fun rps_decode/1, RPS),
    Scores2 = lists:map(fun rps_score/1, RPS2),
    Total2 = lists:sum(Scores2),
    io:format("Part 2: ~p~n", [Total2]).


parse_rps_line(RPSStr) ->
    Line = binary:split(RPSStr, <<" ">>),
    LCLine = lists:map(fun string:lowercase/1, Line),
    AtomLine = lists:map(fun erlang:binary_to_atom/1, LCLine),
    list_to_tuple(AtomLine).

rps_decode({a, x}) -> {a, z};
rps_decode({a, y}) -> {a, x};
rps_decode({a, z}) -> {a, y};
rps_decode({b, _} = Same) -> Same;
rps_decode({c, x}) -> {c, y};
rps_decode({c, y}) -> {c, z};
rps_decode({c, z}) -> {c, x}.

rps_result({a, x}) -> draw;
rps_result({a, y}) -> win;
rps_result({a, z}) -> lose;
rps_result({b, x}) -> lose;
rps_result({b, y}) -> draw;
rps_result({b, z}) -> win;
rps_result({c, x}) -> win;
rps_result({c, y}) -> lose;
rps_result({c, z}) -> draw.

rps_shape_score({_, x}) -> 1;
rps_shape_score({_, y}) -> 2;
rps_shape_score({_, z}) -> 3.

rps_result_score(win) -> 6;
rps_result_score(draw) -> 3;
rps_result_score(lose) -> 0.

rps_score(Game) ->
    rps_result_score(rps_result(Game)) + rps_shape_score(Game).