#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname aoc -mnesia debug verbose
-mode(compile).
-compile([{nowarn_unused_function, [
         {three_words, 2}
        ,{print_all_3_words, 3}
        ,{print_3_word, 3}
        ,{should_print, 2}
        ,{permute, 1}
        ,{permute, 5}
        ,{read_file, 1}
        ,{onoff_with_stops, 1}
        ,{onoff, 1}
        ,{onoff, 2}
        ,{print_onoff, 2}
        ,{print_onoff_all, 2}

    ]}]).

main(_Args) ->
%    onoff_with_stops(Args).
%    three_words([$w, $t, $o, $o, $l, $n, $e, $d], $i).
%    three_words([$d, $s, $i, $a, $e, $e, $w, $n], $c).
%    the_enchantment().
%    the_dream().
    rabid_rat().

rabid_rat() ->
    mutator("m i", [
        1, {around, "aus", "s the"},
        2, {around, "id", "ff a"},
        3, {around, "he r", "s fas"},
        4, {around, "e o", "m o"},
        5, {around, "e t", "t a"},
        6, {around, "pera", "d 'e"},
        7, {},
        8, {},
        9, {},
        10, {},
        ]).

mutator(Start, Mapping = [H|_]) when not is_list(H) ->
    Sequences = lists:uniq(lists:sort(permute(Mapping)))
    Results = lists:map(fun (Seq) -> mutate_seq(Start, Seq) end, Sequences),
    lists:foreach(fun (S) -> io:format("~p~n", S) end, lists:uniq(lists:sort(Results)).

mutate_seq(Start, Seq) ->
    lists:flatten(lists:foldl(fun mutate_fold/2, Start, Seq)).

mutate_fold({around, Prefix, Suffix}, Current) -> [Prefix, Current, Suffix]

the_enchantment() ->
    the_enchantment(lists:uniq(lists:sort(permute([1,2,3,4,5,6,7])))).
the_enchantment([]) ->
    ok;
the_enchantment([Seq|Rest]) ->
    print_enchantment(Seq),
    the_enchantment(Rest).

print_enchantment(E) -> print_enchantment(E, "tn", E).
print_enchantment([], Ans, E) -> io:format("~p      ~p~n", [lists:flatten(Ans), E]);
print_enchantment([Button|Rest], Ans, E) -> print_enchantment(Rest, enchantment_mutate(Button, Ans), E).

enchantment_mutate(1, Ans) -> lists:flatten([Ans, "ee"]);
enchantment_mutate(2, Ans) -> lists:flatten([Ans, "tee"]);
enchantment_mutate(3, Ans) -> lists:flatten(["si", Ans, $n]);
enchantment_mutate(4, Ans) -> lists:flatten([$x, Ans]);
enchantment_mutate(5, Ans) -> lists:flatten(["xt", Ans]);
enchantment_mutate(6, Ans) -> lists:reverse(Ans);
enchantment_mutate(7, Ans) -> lists:flatten(string:replace(Ans, "t", "-si", all)).

the_dream() ->
    the_dream(lists:uniq(lists:sort(permute([12, 12, 3, 4, 5, 6, 7, 8])))).
the_dream([]) ->
    ok;
the_dream([Seq|Rest]) ->
    print_dream(Seq),
    the_dream(Rest).

print_dream(D) -> print_dream(D, "hag", D).
print_dream([], Ans, D) ->
    io:format("~p     ~p~n", [lists:flatten(Ans), D]);
print_dream([Button|Rest], Ans, D) ->
    print_dream(Rest, dream_mutate(Button, Ans), D).

dream_mutate(12, Ans) -> 
    Ans1 = lists:reverse(Ans),
    io:format("D: Ans=~p; Ans2=~p~n", [Ans, Ans1]),
    Ans1;
dream_mutate(3, Ans) -> 
    Ans1 = lists:flatten([$n, Ans, "tegna"]),
    io:format("D: Ans=~p; Ans2=~p~n", [Ans, Ans1]),
    Ans1;
dream_mutate(4, Ans) -> 
    Ans1 = lists:flatten(["osegn", Ans]),
    io:format("D: Ans=~p; Ans2=~p~n", [Ans, Ans1]),
    Ans1;
dream_mutate(5, Ans) -> 
    Ans1 = lists:flatten(["he s", Ans, $g]),
    io:format("D: Ans=~p; Ans2=~p~n", [Ans, Ans1]),
    Ans1;
dream_mutate(6, Ans) -> 
    Ans1 = lists:flatten(["hise", Ans, "ti"]),
    io:format("D: Ans=~p; Ans2=~p~n", [Ans, Ans1]),
    Ans1;
dream_mutate(7, Ans) -> 
    Ans1 = lists:flatten(string:replace(Ans, "g", "nu", all)),
    io:format("D: Ans=~p; Ans2=~p~n", [Ans, Ans1]),
    Ans1;
dream_mutate(8, Ans) -> 
    Ans1 = lists:flatten(string:replace(Ans, "e", " ", all)),
    io:format("D: Ans=~p; Ans2=~p~n", [Ans, Ans1]),
    Ans1.

onoff_with_stops([Elements]) ->
    Unfixed = lists:filter(fun($-) -> false; (_) -> true end, Elements),
    Perms = onoff(Unfixed),
    print_onoff_all(Perms, Elements).

onoff(Switches) ->
    onoff(Switches, [[]]).
onoff([], Results) ->
%    io:format("D: ~p~n", [Results]),
    lists:map(fun lists:reverse/1, Results);
onoff([H|T], Results) ->
%    io:format("D: [~p|~p], ~p~n", [H,T,Results]),
    Ons = lists:map(fun (A) -> [{H,on}|A] end, Results),
    Offs = lists:map(fun (A) -> [{H,off}|A] end, Results),
    Results1 = lists:append(Ons, Offs),
    onoff(T, Results1).

print_onoff_all(Perms, Fixed) ->
    lists:foreach(fun (P) -> print_onoff(P, Fixed) end, Perms).

print_onoff(_Perm, []) -> io:format("~n", []);
print_onoff(Perm, [$-|Fixed]) ->
    io:format("-", []),
    print_onoff(Perm, Fixed);
print_onoff([{H,on}|Perm], [_|Fixed]) ->
    io:format("\033[46m~c\033[0m", [H]),
    print_onoff(Perm, Fixed);
print_onoff([{H,off}|Perm], [_|Fixed]) ->
    io:format("\033[0m~c\033[0m", [H]),
    print_onoff(Perm, Fixed).


three_words(List, Center) ->
    Results = permute(List),
    Permutations = lists:uniq(lists:sort(Results)),
    Dict = read_file("3words.txt"),
    print_all_3_words(Permutations, Center, Dict).

print_all_3_words([H|[]], Center, Dict) ->
    print_3_word(H, Center, Dict);
print_all_3_words([H|T], Center, Dict) ->
    print_3_word(H, Center, Dict),
    %io:format("~n", []),
    print_all_3_words(T, Center, Dict).

print_3_word([L1, L2, L3,
              L4,     L5,
              L6, L7, L8], Center, Dict) ->
    WordsAcross = [[L1, L2, L3], [L4, Center, L5], [L6, L7, L8]],
    WordsDown = [[L1, L4, L6], [L2, Center, L7], [L3, L5, L8]],
    case should_print(WordsAcross, Dict) andalso should_print(WordsDown, Dict) of
        true ->
            io:format("~s~n~s~n~s~n~n", WordsAcross);
        false ->
            ok
    end.

should_print([], _Dict) -> true;
should_print([H|T], Dict) ->
    case lists:member(H, Dict) of
        true -> should_print(T, Dict);
        false -> false
    end.

permute(List) ->
    permute(List, 1, [], length(List), []).

permute([], _Index, Result, Target, AllResult) when length(Result) =:= Target ->
    [lists:reverse(Result)|AllResult];
permute([], _Index, _Result, _Target, AllResult) ->
    AllResult;
permute(Remaining, Index, _Result, _Target, AllResult) when Index > length(Remaining) ->
    AllResult;
permute(Remaining, Index, Result, Target, AllResult) ->
    {Head, Tail} = lists:split(Index - 1, Remaining),
    [Zap|Zapped] = Tail,
    AllResult1 = permute(lists:append(Head, Zapped), 1, [Zap|Result], Target, AllResult),
    permute(Remaining, Index + 1, Result, Target, AllResult1).

read_file(Name) ->
    {ok, Data} = file:read_file(Name),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    lists:map(fun erlang:binary_to_list/1, Lines).