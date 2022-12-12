-module(aoc_problem_11).
-export([solve/2, monkey_actor/2]).

solve(Problem, Example) ->
    Monkeys = aoc_timer:input(fun ingest/2, [Problem, Example]),
    aoc_timer:problem(1, fun solve_1/1, [Monkeys]),
    fprof:apply(aoc_timer, problem, [2, fun solve_2/1, [Monkeys]]).
%    aoc_timer:problem(2, fun solve_2/1, [Monkeys]).

ingest(Problem, Example) ->
    Monkeys = aoc_input:split_by_blanks(Problem, Example),
    {ok, Re} = re:compile("Monkey (\\d+):\\s+Starting items: ([0-9 ,]+)\\s+Operation: new = old (.) (\\d+|old)\\s+Test: divisible by (\\d+)\\s+If true: throw to monkey (\\d+)\\s+If false: throw to monkey (\\d+)", [anchored, dotall, multiline]),
    lists:map(fun (Monk) ->
        % io:format("Monk=~p~nRe=~p~n", [Monk, Re]),
        {match, [MonkeyId, StartingItems, MonkeyOp, MonkeyOpN, TestDivN, TrueId, FalseId]} = re:run(Monk, Re, [{capture, all_but_first, binary}]),
        #{
            id => binary_to_atom(<<"m",MonkeyId/binary>>),
            inventory => lists:map(fun erlang:binary_to_integer/1, binary:split(StartingItems, <<", ">>, [global])),
            operation => {binary_to_atom(MonkeyOp), if MonkeyOpN =:= <<"old">> -> old; true -> binary_to_integer(MonkeyOpN) end},
            test => {binary_to_integer(TestDivN), binary_to_atom(<<"m",TrueId/binary>>), binary_to_atom(<<"m", FalseId/binary>>)}
        }
    end, Monkeys).

solve_1(MonkeyDefs) ->
    MonkeyDefs2 = lists:map(fun (MonkeyDef) -> MonkeyDef#{worry_decay => 3} end, MonkeyDefs),
    Monkeys = lists:map(fun spawn_monkey/1, MonkeyDefs2),
    MonkeyCounter = maps:from_keys(Monkeys, 0),
    Result = coordinate_turns(Monkeys, MonkeyCounter, 20),
    lists:foreach(fun (Monkey) -> Monkey ! terminate end, Monkeys),
    Result.

solve_2(MonkeyDefs) ->
    MonkeyDefs2 = lists:map(fun (MonkeyDef) ->
        MonkeyDef#{worry_decay => 1}
    end, MonkeyDefs),
    Monkeys = lists:map(fun spawn_monkey/1, MonkeyDefs2),
    MonkeyCounter = maps:from_keys(Monkeys, 0),
    coordinate_turns(Monkeys, MonkeyCounter, 10_000).

spawn_monkey(#{id := Id} = MonkeyDef) -> 
    Pid = spawn(?MODULE, monkey_actor, [MonkeyDef, self()]),
    register(Id, Pid),
    Id.

monkey_actor(#{inventory := Inv, id := Id} = Def, Parent) ->
    receive
        take_turn ->
            % io:format("Monkey ~p: take a turn~n", [Id]),
            Def2 = monkey_turn(Def),
            Parent ! {turn_finished, Id, length(Inv)},
            monkey_actor(Def2, Parent);
        {item, Item} ->
            % io:format("Monkey ~p: get item ~p~n", [Id, Item]),
            Def2 = monkey_receive(Item, Def),
            monkey_actor(Def2, Parent);
        terminate ->
            ok
    end.

monkey_turn(#{inventory := []} = Def) ->
    Def;
monkey_turn(#{inventory := [Item|Inventory], operation := Op, worry_decay := Decay, test := Test} = Def) ->
    Item2 = monkey_inspect(Item, Op),
    Item3 = Item2 div Decay,
    monkey_test(Item3, Test),
    monkey_turn(Def#{inventory => Inventory}).

monkey_inspect(Item, {Op, old}) ->
    erlang:Op(Item, Item);
monkey_inspect(Item, {Op, N}) ->
    erlang:Op(Item, N).

monkey_test(Item, {Test, True, False}) ->
    case Item rem Test of
        0 -> True ! {item, Item};
        _ -> False ! {item, Item}
    end.

monkey_receive(Item, #{inventory := Inventory} = Def) ->
    Def#{inventory => lists:append(Inventory, [Item])}.

coordinate_turns(_Monkeys, MonkeyCounter, 0) ->
    [M1, M2 | _] = lists:reverse(lists:sort(maps:values(MonkeyCounter))),
    M1 * M2;
coordinate_turns(Monkeys, MonkeyCounter, Turns) ->
    case Turns rem 100 of 
        0 ->
            io:format("Turns left: ~p~n", [Turns]);
        _ -> ok end,
    MonkeyCounter2 = coordinate_turn(Monkeys, MonkeyCounter),
    coordinate_turns(Monkeys, MonkeyCounter2, Turns-1).

coordinate_turn([], MonkeyCounter) ->
    MonkeyCounter;
coordinate_turn([Monkey|Monkeys], MonkeyCounter) ->
    Monkey ! take_turn,
    MonkeyCounter2 = receive 
        {turn_finished, Monkey, Inspections} ->
            OldCount = maps:get(Monkey, MonkeyCounter),
            MonkeyCounter#{Monkey => OldCount + Inspections}
    end,
    coordinate_turn(Monkeys, MonkeyCounter2).