#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname aoc -mnesia debug verbose
-mode(compile).

main(_) ->
    Tokens = read_input("10"),
    %io:format("Tokens=~p~n", [Tokens]),
    Parsed = lists:map(fun parse/1, Tokens),
    io:format("Parsed=~p~n", [Parsed]),
    ErrorScore = score_errors(Parsed),
    io:format("Error score ~p~n", [ErrorScore]),
    MedianIncomplete = score_incompletes(Parsed),
    io:format("Median incomplete score ~p~n", [MedianIncomplete]),
    ok.

score_errors(Parsed) ->
    Errors = lists:filter(fun ({error, _, _}) -> true; (_) -> false end, Parsed),
    ErrorTokens = lists:map(fun ({error, _, T}) -> T end, Errors),
    ErrorScores = lists:map(fun error_score_for_token/1, ErrorTokens),
    lists:sum(ErrorScores).

error_score_for_token(<<")">>) -> 3;
error_score_for_token(<<"]">>) -> 57;
error_score_for_token(<<"}">>) -> 1197;
error_score_for_token(<<">">>) -> 25137.

incomplete_score_for_token(<<")">>) -> 1;
incomplete_score_for_token(<<"]">>) -> 2;
incomplete_score_for_token(<<"}">>) -> 3;
incomplete_score_for_token(<<">">>) -> 4.

close_for_open(<<"(">>) -> <<")">>;
close_for_open(<<"[">>) -> <<"]">>;
close_for_open(<<"{">>) -> <<"}">>;
close_for_open(<<"<">>) -> <<">">>.

score_incompletes(Parsed) ->
    Incompletes = lists:filter(fun ({incomplete, _}) -> true; (_) -> false end, Parsed),
    Completions = lists:map(fun complete_line/1, Incompletes),
    CompletionScores = lists:map(fun score_missing_tokens/1, Completions),
    Sorted = lists:sort(CompletionScores),
    io:format("Incomplete scores ~p~n", [Sorted]),
    1 = (length(Sorted) rem 2),
    lists:nth(round(length(Sorted)/2), Sorted).


complete_line({incomplete, Stack}) ->
    lists:map(fun close_for_open/1, Stack).

score_missing_tokens(LineEnd) ->
    TokenScores = lists:map(fun incomplete_score_for_token/1, LineEnd),
    lists:foldl(fun (TokenScore, Acc) -> (Acc * 5) + TokenScore end, 0, TokenScores).

parse(Line) ->
    % io:format("Parsing line ~p~n", [Line]),
    parse(Line, []).
parse([], []) ->
    ok;
parse([], Stack) ->
    {incomplete, Stack};
parse([Opener|Line], Stack) when Opener =:= <<"(">> orelse Opener =:= <<"<">> orelse Opener =:= <<"{">> orelse Opener =:= <<"[">> ->
    % io:format("Opener=~p Stack=~p~n", [Opener, Stack]),
    parse(Line, [Opener|Stack]);
parse([<<")">>|Line], [<<"(">>|Stack]) ->
    % io:format("Closed~n"),
    parse(Line, Stack);
parse([<<"}">>|Line], [<<"{">>|Stack]) ->
    % io:format("Closed~n"),
    parse(Line, Stack);
parse([<<"]">>|Line], [<<"[">>|Stack]) ->
    % io:format("Closed~n"),
    parse(Line, Stack);
parse([<<">">>|Line], [<<"<">>|Stack]) ->
    % io:format("Closed~n"),
    parse(Line, Stack);
parse([Closer|_], [Expected|_]) ->
    % io:format("Error Expected=~p Closer=~p~n", [Expected, Closer]),
    {error, Expected, Closer}.

read_input(Name) ->
    {ok, Data} = file:read_file(Name),
    Lines = re:split(Data, "\n", [trim]),
    lists:map(fun (L) -> re:split(L, "", [trim]) end, Lines).
