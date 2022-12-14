-module(aoc_problem_13).
-export([solve/2]).

solve(Problem, Example) ->
    InputPairs = aoc_timer:input(fun ingest/2, [Problem, Example]),
    aoc_timer:problem(1, fun solve_1/1, [InputPairs]),
    % fprof:apply(aoc_timer, problem, [2, fun solve_2/1, [Monkeys]]).
    aoc_timer:problem(2, fun solve_2/1, [InputPairs]).

ingest(Problem, Example) ->
    Pairs = aoc_input:split_by_blanks(Problem, Example),
    lists:map(fun (PairStr) ->
        [Left, Right] = binary:split(PairStr, <<"\n">>),
        {ingest_expr(Left), ingest_expr(Right)}
    end, Pairs).

ingest_expr(Binary) ->
    String = binary_to_list(<<Binary/binary, ".">>),
    {ok, Tokens, _} = erl_scan:string(String),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:exprs(Exprs, []),
    Value.

solve_1(Pairs) ->
    score_correct_order(Pairs, 1, 0).

score_correct_order([], _, Sum) ->
    Sum;
score_correct_order([{L,R}|Pairs], Index, Sum) ->
    Sum2 = case correctly_ordered(L,R) of
        true -> Sum + Index;
        false -> Sum
    end,
    score_correct_order(Pairs, Index+1, Sum2).

correctly_ordered([], []) ->
    undefined;
correctly_ordered([], [_|_]) -> 
    true;
correctly_ordered([_|_], []) ->
    false;
correctly_ordered([N|RestL], [N|RestR]) ->
    correctly_ordered(RestL, RestR);
correctly_ordered([NL|_], [NR|_]) when is_integer(NL) andalso is_integer(NR) ->
    NL < NR;
correctly_ordered([NL|RestL], [NR|RestR]) when is_list(NL) andalso is_list(NR) ->
    case correctly_ordered(NL, NR) of
        true -> true;
        false -> false;
        undefined -> correctly_ordered(RestL, RestR)
    end;
correctly_ordered([NL|RestL], [NR|RestR]) when is_list(NL) andalso is_integer(NR) ->
    correctly_ordered([NL|RestL], [[NR]|RestR]);
correctly_ordered([NL|RestL], [NR|RestR]) when is_integer(NL) andalso is_list(NR) ->
    correctly_ordered([[NL]|RestL], [NR|RestR]).

solve_2(Pairs) ->
    InputItems = lists:flatmap(fun tuple_to_list/1, Pairs),
    Sorted = lists:sort(fun correctly_ordered/2, [[[2]], [[6]]|InputItems]),
    [A,B] = lists:filtermap(fun ({Index, Element}) -> 
        if 
            Element =:= [[2]] -> {true, Index};
            Element =:= [[6]] -> {true, Index};
            true -> false
        end 
    end, lists:enumerate(Sorted)),
    A*B.