#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname aoc -mnesia debug verbose
-mode(compile).

main(_) ->
    {Numbers, Tables} = read_input("4"),
    io:format("Numbers: ~p; Tables: ~p~n", [Numbers, Tables]),
    store_tables_ets(Tables),
    [Score] = play(Numbers),
    io:format("First win score: ~p~n", [Score]),
    LastScore = play_to_end([Score]),
    io:format("Last win score: ~p~n", [LastScore]),
    ok.

play_to_end([#{leftover_numbers := Numbers}] = [PrevScore]) ->
    case ets:info(tabs, size) of
        1 -> 
            PrevScore;
        N when is_integer(N) -> 
            delete_board(PrevScore),
            NextScore = play(Numbers),
            io:format("Board completed: ~p~n", [NextScore]),
            play_to_end(NextScore)
    end;
play_to_end([Score|Scores]) ->
    io:format("Cleaning up boards: ~p~n", [Score]),
    delete_board(Score),
    play_to_end(Scores).

delete_board(#{board := FinishedBoard}) ->
    ets:delete(tabs, FinishedBoard),
    lists:foreach(
        fun ({N, Targets}) ->
            PrunedTargets = lists:foldr(
                fun ({tabs, B}, L) when B =:= FinishedBoard ->
                    % io:format("tab match L=~p~n", [L]),
                    L
                ; ({cols, B, _}, L) when B =:= FinishedBoard ->
                    % io:format("col match L=~p~n", [L]),
                    L
                ; ({rows, B, _}, L) when B =:= FinishedBoard ->
                    % io:format("row match L=~p~n", [L]),
                    L
                ; (Other, L) ->
                    % io:format("nil match O=~p L=~p~n", [Other, L]),
                    [Other|L]
                end,
                [],
                Targets),
            % io:format("Targets: ~p->~p~n", [Targets, PrunedTargets]),
            ets:insert(nums, {N, PrunedTargets})
        end,
        ets:tab2list(nums)).

score(N, Board, Rest) ->
    [[Remains]] = ets:match(tabs, {Board, '$1'}),
    BoardScore = lists:sum(Remains),
    #{board => Board, board_score => BoardScore, number_score => N, total => BoardScore * N, leftover_numbers => Rest}.

play([N|Rest]) ->
    case play_number(N) of
        [] -> play(Rest);
        Boards -> [score(N, Board, Rest) || Board <- Boards]
    end.

play_number(N) ->
    [[Targets]] = ets:match(nums, {N, '$1'}),
    play_targets(N, Targets, []).

play_targets(_, [], Response) ->
    Response;
play_targets(N, [{tabs, T}|Rest], Response) ->
    [[TableItems]] = ets:match(tabs, {T, '$1'}),
    NewTableItems = lists:delete(N, TableItems),
    ets:insert(tabs, {T, NewTableItems}),
    play_targets(N, Rest, Response);
play_targets(N, [{cols, T, C}|Rest], Response) ->
    [[ColItems]] = ets:match(cols, {{T, C}, '$1'}),
    NewColItems = lists:delete(N, ColItems),
    NewResponse = case NewColItems of [] -> lists:umerge([T],Response); _ -> Response end,
    ets:insert(cols, {{T, C}, NewColItems}),
    io:format("N=~p T=~p C=~p Items=~p->~p Response=~p->~p~n", [N, T, C, ColItems, NewColItems, Response, NewResponse]),
    play_targets(N, Rest, NewResponse);
play_targets(N, [{rows, T, R}|Rest], Response) ->
    [[RowItems]] = ets:match(rows, {{T, R}, '$1'}),
    NewRowItems = lists:delete(N, RowItems),
    NewResponse = case NewRowItems of [] -> lists:umerge([T],Response); _ -> Response end,
    ets:insert(rows, {{T, R}, NewRowItems}),
    io:format("N=~p T=~p R=~p Items=~p->~p Response=~p->~p~n", [N, T, R, RowItems, NewRowItems, Response, NewResponse]),
    play_targets(N, Rest, NewResponse).

read_input(Name) ->
    {ok, Data} = file:read_file(Name),
    [Numbers | Tables] = re:split(Data, "\n\n+", [trim]),
    Numbers1 = parse_numbers(Numbers),
    Tables1 = parse_tables(Tables),
    {Numbers1, Tables1}.

parse_numbers(NumbersBin) ->
    Split = re:split(NumbersBin, ",", [trim]),
    [binary_to_integer(N) || N <- Split].

parse_tables(TablesBin) ->
    lists:zipwith(fun (T, I) -> {table, I, parse_table(T)} end, TablesBin, lists:seq(1, length(TablesBin))).

parse_table(Table) ->
    %io:format("Table: ~p~n", [Table]),
    [parse_row(R) || R <- re:split(Table, <<" *\n *">>, [trim])].

parse_row(Row) ->
    %io:format("Row: ~p~n", [Row]),
    [binary_to_integer(N) || N <- re:split(Row, " +", [trim]), size(N) > 0].

store_tables_ets(Tables) ->
    ets:new(nums, [named_table]),
    ets:new(rows, [named_table]),
    ets:new(cols, [named_table]),
    ets:new(tabs, [named_table]),
    lists:foreach(fun store_table_ets/1, Tables).

store_table_ets({table, N, Rows}) ->
    ets:insert_new(tabs, {N, lists:sort(lists:flatten(Rows))}),
    store_rows_ets(N, 1, Rows).

store_rows_ets(_, _, []) ->
    ok;
store_rows_ets(TableId, RowId, [Row|Rows]) ->
    store_cols_ets(TableId, RowId, 1, Row),
    store_rows_ets(TableId, RowId+1, Rows).

store_cols_ets(_, _, _, []) ->
    ok;
store_cols_ets(TableId, RowId, ColId, [Col|Cols]) ->
    store_col_ets(TableId, RowId, ColId, Col),
    store_cols_ets(TableId, RowId, ColId+1, Cols).

store_col_ets(TableId, RowId, ColId, Num) ->
    ets:insert_new(nums, {Num, []}),
    [[Locations]] = ets:match(nums, {Num, '$1'}),
    AddLocations = [{tabs, TableId}, {rows, TableId, RowId}, {cols, TableId, ColId}],
    ets:insert(nums, {Num, lists:umerge(lists:sort(AddLocations), Locations)}),

    ets:insert_new(rows, {{TableId, RowId}, []}),
    [[RowItems]] = ets:match(rows, {{TableId, RowId}, '$1'}),
    ets:insert(rows, {{TableId, RowId}, lists:umerge([Num], RowItems)}),

    ets:insert_new(cols, {{TableId, ColId}, []}),
    [[ColItems]] = ets:match(cols, {{TableId, ColId}, '$1'}),
    ets:insert(cols, {{TableId, ColId}, lists:umerge([Num], ColItems)}).