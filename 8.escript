#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname aoc -mnesia debug verbose
-mode(compile).

main(_) ->
    Input = read_input("8"),
    % io:format("Input: ~p~n", [Input]),
    % Part1 = part1_answer(Input),
    % io:format("Part1: ~p~n", [Part1]),
    Part2 = lists:sum(lists:map(fun decode_line/1, Input)),
    io:format("Part2: ~p~n", [Part2]),
    ok.

-define(ZERO,   [a,b,c,e,f,g]).
-define(ONE,    [c,f]).
-define(TWO,    [a,c,d,e,g]).
-define(THREE,  [a,c,d,f,g]).
-define(FOUR,   [b,c,d,f]).
-define(FIVE,   [a,b,d,f,g]).
-define(SIX,    [a,b,d,e,f,g]).
-define(SEVEN,  [a,c,f]).
-define(EIGHT,  [a,b,c,d,e,f,g]).
-define(NINE,   [a,b,c,d,f,g]).

decode_line(#{input := I, output := O}) ->
    AllData = lists:usort(lists:append(I, O)),
    io:format("AllData=~p~n", [AllData]),
    CouldBeAny = ?EIGHT,
    PartialAnswer = partial_decode_line(AllData, 
        #{
            a => CouldBeAny,
            b => CouldBeAny,
            c => CouldBeAny,
            d => CouldBeAny,
            e => CouldBeAny,
            f => CouldBeAny,
            g => CouldBeAny
        }),
    Solved = maps:map(fun (_K, [V]) -> V end, maps:filter(fun (_K,V) -> length(V) =:= 1 end, PartialAnswer)),
    Unsolved = maps:filter(fun (_K,V) -> length(V) =/= 1 end, PartialAnswer),
    io:format("Partial solution: Solved=~p Unsolved=~p~n", [Solved, Unsolved]),
    AnswerKey = search_answer(AllData, Unsolved, Solved),
    translate_digits(O, AnswerKey).

search_answer(AllData, Unsolved, Solved) when map_size(Unsolved) =:= 0 ->
    % Flip solved to be observed -> actual
    Solved1 = maps:from_list(lists:map(fun ({K,V}) -> {V,K} end, maps:to_list(Solved))),
    case verify_answer(AllData, Solved1) of
        true -> Solved1;
        false -> backtrack
    end;
search_answer(AllData, Unsolved, Solved) ->
    % io:format("Find least values of ~p~n", [Unsolved]),
    LeastValues = find_least_values(Unsolved),
    % io:format("Looking at ~p because it has fewest options~n", [LeastValues]),
    try_values(AllData, Unsolved, Solved, LeastValues).

try_values(_, _, _, {_, []}) ->
    backtrack;
try_values(AllData, Unsolved, Solved, {Key, [Value|Rest]}) ->
    {MaybeUnsolved, MaybeSolved} = set_mapping(Key, Value, Unsolved, Solved),
    case search_answer(AllData, MaybeUnsolved, MaybeSolved) of
        backtrack -> try_values(AllData, Unsolved, Solved, {Key, Rest});
        Solution -> Solution
    end.

set_mapping(Key, Value, Unsolved, Solved) ->
    Solved1 = maps:put(Key, Value, Solved),
    Unsolved1 = maps:remove(Key, Unsolved),
    Unsolved2 = maps:map(fun (_K,Values) -> lists:delete(Value, Values) end, Unsolved1),
    {Unsolved2, Solved1}.

verify_answer(AllData, Solved) ->
    io:format("Attempting to verify the solution ~p~n", [Solved]),
    Possible = [?ZERO, ?ONE, ?TWO, ?THREE, ?FOUR, ?FIVE, ?SIX, ?SEVEN, ?EIGHT, ?NINE],
    verify_answer(AllData, Solved, Possible, Possible).
verify_answer([], _, [], _) ->
    true;
verify_answer([], _, Undiscovered, _) ->
    io:format("Solution looks valid, but never discovered ~p~n", [Undiscovered]),
    true;
verify_answer([Digit|Rest], Solved, Available, Possible) ->
    Translated = translate(Digit, Solved),
    case lists:member(Translated, Possible) of
        true ->
            case lists:delete(Translated, Available) of
                LessAvailable when LessAvailable =:= Available ->
                    io:format("Failure: we already got a copy of ~p~n", [Translated]),
                    false;
                LessAvailable -> 
                    io:format("OK so far. Eliminating ~p~n", [Translated]),
                    verify_answer(Rest, Solved, LessAvailable, Possible)
            end;
        false ->
            io:format("~p doesn't match any digit so it's invalid~n", [Translated]),
            false
    end.

translate_digits(Digits, Key) ->
    NumList = lists:map(
        fun (Digit) -> 
            case translate(Digit, Key) of
                ?ZERO -> "0";
                ?ONE -> "1";
                ?TWO -> "2";
                ?THREE -> "3";
                ?FOUR -> "4";
                ?FIVE -> "5";
                ?SIX -> "6";
                ?SEVEN -> "7";
                ?EIGHT -> "8";
                ?NINE -> "9"
            end
        end, 
    Digits),
    list_to_integer(lists:concat(NumList)).

translate(Digit, Key) ->
    Translated = lists:map(
        fun (El) ->
            maps:get(El, Key)
        end,
        Digit),
    lists:sort(Translated).

find_least_values(Map) ->
    I1 = maps:iterator(Map),
    {K, V, I2} = maps:next(I1),
    find_least_values(maps:next(I2), {K, V}).
find_least_values(none, Winner) ->
    Winner;
find_least_values(Next, {_K, V} = Prev) ->
    case Next of
        {K1, V1, I} when length(V1) < length(V) -> 
            find_least_values(maps:next(I), {K1, V1});
        {_, _, I} ->
            find_least_values(maps:next(I), Prev)
    end.

%%% Use the rules about known digits to greatly reduce the search space %%%
partial_decode_line(_, #{a := [_], b := [_], c := [_], d := [_], e := [_], f := [_], g := [_]} = Ans) ->
    Ans;
partial_decode_line([], Ans) ->
    Ans;
partial_decode_line([Digit|Rest], Ans) ->
    Ans1 = decode_digit(Digit, Ans),
    % io:format("Decoded ~p for ~p -> ~p~n", [Digit, Ans, Ans1]),
    partial_decode_line(Rest, Ans1).

digit_focus_options(ValidPositions, Digit, Ans) ->
    Ans1 = digit_limit_to_possible(ValidPositions, Digit, Ans),
    InvalidPositions = lists:subtract([a,b,c,d,e,f,g], ValidPositions),
    digit_remove_options(InvalidPositions, Digit, Ans1).

digit_limit_to_possible([], _Letters, Ans) ->
    Ans;
digit_limit_to_possible([Target|Targets], Letters, Ans) ->
    Ans1 = maps:update_with(
        Target,
        fun (V) ->
            lists:filter(fun (E) -> lists:member(E, Letters) end, V)
        end,
        Ans),
    digit_limit_to_possible(Targets, Letters, Ans1).

digit_remove_options([], _Letters, Ans) ->
    Ans;
digit_remove_options([Target|Targets], Letters, Ans) ->
    Ans1 = maps:update_with(
        Target, 
        fun (V) -> 
            L = lists:subtract(V, Letters),
            % io:format("~p - ~p = ~p~n", [V, Letters, L]),
            L
        end, 
        Ans),
    % io:format("removing options ~p -> ~p~n", [ Ans, Ans1]),
    digit_remove_options(Targets, Letters, Ans1).

decode_digit(Digit, Ans) when length(Digit) =:= 7 ->
    Ans;  % 8, nothing eliminated
decode_digit(Digit, Ans) when length(Digit) =:= 6 ->
    Ans;  % 0, 6, or 9
decode_digit(Digit, Ans) when length(Digit) =:= 5 ->
    Ans;  % 2, 3, or 5
decode_digit(Digit, Ans) when length(Digit) =:= 4 ->
    digit_focus_options(?FOUR, Digit, Ans); % 4 only
decode_digit(Digit, Ans) when length(Digit) =:= 3 ->
    digit_focus_options(?SEVEN, Digit, Ans);  % 7 only
decode_digit(Digit, Ans) when length(Digit) =:= 2 ->
    digit_focus_options(?ONE, Digit, Ans).  % 1 only


part1_answer(Lines) ->
    Count = lists:map(fun part1_line_count/1, Lines),
    lists:sum(Count).

part1_line_count(#{output := Line}) ->
    Counts = lists:map(fun erlang:length/1, Line),
    FilteredCounts = lists:filter(fun 
        (E) when E =:= 2; E =:= 4; E =:= 3; E =:= 7 -> true; 
        (_E) -> false 
    end, Counts),
    length(FilteredCounts).

parse_input_string(Binary) ->
    List = re:split(Binary, "", [trim]),
    Sorted = lists:sort(List),
    lists:map(fun erlang:binary_to_atom/1, Sorted).


read_input(Name) ->
    {ok, Data} = file:read_file(Name),
    Lines = re:split(Data, "\n", [trim]),
    IO = lists:map(fun (E) -> 
        list_to_tuple(binary:split(E, <<" | ">>))
    end, Lines),
    lists:map(fun ({I, O}) -> 
        I1 = binary:split(I, <<" ">>, [global]),
        O1 = binary:split(O, <<" ">>, [global]),
        I2 = lists:map(fun parse_input_string/1, I1),
        O2 = lists:map(fun parse_input_string/1, O1),
        #{input => I2, output => O2}
    end, IO).
