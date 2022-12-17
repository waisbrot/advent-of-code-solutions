-module(aoc_problem_11).
-export([solve/2, monkey_actor/2]).
-include_lib("kernel/include/logger.hrl").

solve(Problem, Example) ->
    ok = logger:update_primary_config(#{level => debug}),
    logger:add_primary_filter(debug, {fun (#{meta := #{debug := true}}, _) -> ignore; (_, _) -> stop end, ok}),
    Monkeys = aoc_timer:input(fun ingest/2, [Problem, Example]),
    aoc_timer:problem(1, fun solve_1/1, [Monkeys]),
    % fprof:apply(aoc_timer, problem, [2, fun solve_2/1, [Monkeys]]).
    aoc_timer:problem(2, fun solve_2/1, [Monkeys]).

ingest(Problem, Example) ->
    Monkeys = aoc_input:split_by_blanks(Problem, Example),
    {ok, Re} = re:compile("Monkey (\\d+):\\s+Starting items: ([0-9 ,]+)\\s+Operation: new = old (.) (\\d+|old)\\s+Test: divisible by (\\d+)\\s+If true: throw to monkey (\\d+)\\s+If false: throw to monkey (\\d+)", [anchored, dotall, multiline]),
    lists:map(fun (Monk) ->
        % io:format("Monk=~p~nRe=~p~n", [Monk, Re]),
        {match, [MonkeyId, StartingItems, MonkeyOp, MonkeyOpN, TestDivN, TrueId, FalseId]} = re:run(Monk, Re, [{capture, all_but_first, binary}]),
        #{
            id => binary_to_atom(<<"m",MonkeyId/binary>>),
            inventory => queue:from_list(lists:map(fun erlang:binary_to_integer/1, binary:split(StartingItems, <<", ">>, [global]))),
            operation => {binary_to_atom(MonkeyOp), if MonkeyOpN =:= <<"old">> -> old; true -> binary_to_integer(MonkeyOpN) end},
            test => {binary_to_integer(TestDivN), binary_to_atom(<<"m",TrueId/binary>>), binary_to_atom(<<"m", FalseId/binary>>)}
        }
    end, Monkeys).

solve_1(MonkeyDefs) ->
    MonkeyDefs2 = lists:map(fun (MonkeyDef) -> MonkeyDef#{worry_decay => 3, lcd => 1} end, MonkeyDefs),
    Monkeys = lists:map(fun spawn_monkey/1, MonkeyDefs2),
    MonkeyCounter = maps:from_keys(Monkeys, 0),
    Result = coordinate_turns(Monkeys, MonkeyCounter, 20),
    lists:foreach(fun (Monkey) -> Monkey ! terminate end, Monkeys),
    Result.

solve_2(MonkeyDefs) ->
    CommonDenom = lists:foldl(fun (#{test := {Denom, _, _}}, LCD) -> Denom * LCD end, 1, MonkeyDefs),
    MonkeyDefs2 = lists:map(fun (MonkeyDef) ->
        MonkeyDef#{worry_decay => 1, lcd => CommonDenom}
    end, MonkeyDefs),
    io:format("LCD=~p~n", [CommonDenom]),
    Monkeys = lists:map(fun spawn_monkey/1, MonkeyDefs2),
    MonkeyCounter = maps:from_keys(Monkeys, 0),
    Result = coordinate_turns(Monkeys, MonkeyCounter, 10_000),
    lists:foreach(fun (Monkey) -> Monkey ! terminate end, Monkeys),
    Result.

spawn_monkey(#{id := Id} = MonkeyDef) -> 
    Pid = spawn(?MODULE, monkey_actor, [MonkeyDef, self()]),
    register(Id, Pid),
    Id.

monkey_actor(#{inventory := Inv, id := Id} = Def, Parent) ->
    receive
        {take_turn, DebugTurn} ->
            % ?LOG_DEBUG("Monkey ~p: take a turn~n", [Id], #{debug => DebugTurn}),
            Def2 = monkey_turn(Def, DebugTurn),
            Parent ! {turn_finished, Id, queue:len(Inv)},
            monkey_actor(Def2, Parent);
        {item, Item, DebugTurn} ->
            % ?LOG_DEBUG("Monkey ~p: get item ~p~n", [Id, Item], #{debug => DebugTurn}),
            Def2 = monkey_receive(Item, Def),
            monkey_actor(Def2, Parent);
        terminate ->
            ok
    end.

monkey_turn(#{inventory := Inventory, operation := Op, worry_decay := Decay, ldc := LDC, test := Test} = Def, DebugTurn) ->
    case queue:out(Inventory) of
        {empty, _} -> 
            Def;
        {{value, Item}, Inventory2} ->
            Item2 = monkey_inspect(Item, Op),
            Item3 = Item2 div Decay,
            Item4 = Item3 rem LDC,
            monkey_test(Item4, Test, DebugTurn),
            monkey_turn(Def#{inventory => Inventory2}, DebugTurn)
    end.

monkey_inspect(Item, {Op, old}) ->
    erlang:Op(Item, Item);
monkey_inspect(Item, {Op, N}) ->
    erlang:Op(Item, N).

monkey_test(Item, {Test, True, False}, DebugTurn) ->
    case Item rem Test of
        0 -> True ! {item, Item, DebugTurn};
        _ -> False ! {item, Item, DebugTurn}
    end.

monkey_receive(Item, #{inventory := Inventory} = Def) ->
    Def#{inventory => queue:in(Item, Inventory)}.

coordinate_turns(_Monkeys, MonkeyCounter, 0) ->
    [M1, M2 | _] = lists:reverse(lists:sort(maps:values(MonkeyCounter))),
    io:format("Top monkeys: ~p * ~p = ~p~n", [M1, M2, M1*M2]),
    M1 * M2;
coordinate_turns(Monkeys, MonkeyCounter, Turns) ->
    DebugTurn = case Turns rem 100 of 
        0 ->
            ?LOG_DEBUG("Turns left: ~p~n", [Turns], #{debug => false}),
            true;
        _ -> false
    end,
    MonkeyCounter2 = coordinate_turn(Monkeys, MonkeyCounter, DebugTurn),
    coordinate_turns(Monkeys, MonkeyCounter2, Turns-1).

coordinate_turn([], MonkeyCounter, _) ->
    MonkeyCounter;
coordinate_turn([Monkey|Monkeys], MonkeyCounter, DebugTurn) ->
    Monkey ! {take_turn, DebugTurn},
    MonkeyCounter2 = receive 
        {turn_finished, Monkey, Inspections} ->
            OldCount = maps:get(Monkey, MonkeyCounter),
            % io:format("Monkey ~p count ~p + ~p~n", [Monkey, OldCount, Inspections]),
            MonkeyCounter#{Monkey => OldCount + Inspections}
    end,
    coordinate_turn(Monkeys, MonkeyCounter2, DebugTurn).