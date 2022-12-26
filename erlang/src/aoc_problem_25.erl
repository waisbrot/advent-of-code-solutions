-module(aoc_problem_25).
-export([solve/2]).

solve(Problem, Example) ->
    Numbers = aoc_timer:input(fun ingest/2, [Problem, Example]),
    aoc_timer:problem(1, fun solve_1/1, [Numbers]),
    aoc_timer:problem(2, fun solve_2/1, [Numbers]).

ingest(Problem, Example) ->
    Lines = aoc_input:split_by_lines(Problem, Example),
    lists:map(fun (Line) -> parse_line(lists:reverse(binary_to_list(Line)), 0, 0) end, Lines).

parse_line([], _, Sum) ->
    Sum;
parse_line([N|Rest], Place, Sum) ->
    Mult = math:pow(5, Place),
    parse_line(Rest, Place+1, Sum + Mult * char_to_number(N)).

char_to_number($-) -> -1;
char_to_number($=) -> -2;
char_to_number($0) -> 0;
char_to_number($1) -> 1;
char_to_number($2) -> 2.

solve_1(Numbers) ->
    Total = lists:sum(Numbers),
    MaxPlace = find_max_place(Total),
    lists:reverse(to_snaf(Total, MaxPlace, [])).

find_max_place(N) ->
    find_max_place(N, 0, 0).
find_max_place(N, Place, PrevMax) ->
    One = math:pow(5, Place),
    Two = One * 2,
    Max = PrevMax+Two,
    if
        Max < N -> find_max_place(N, Place+1, Max);
        true -> Place
    end.

max_at_place(0, Sum) -> Sum;
max_at_place(N, Sum) -> max_at_place(N-1, Sum + math:pow(5, N)*2).

to_snaf(0.0, -1, Acc) ->
    Acc;
to_snaf(0, -1, Acc) ->
    Acc;
to_snaf(_, -1, _) ->
    no;
to_snaf(Total, Exp, Acc) ->
    case max_at_place(Exp, 0) of
        N when N < Total -> 
            no;
        N when -N > Total -> 
            no;
        _ ->
            case lists:filter(fun erlang:is_list/1, snaf_guesses(Total, Exp, Acc)) of
                [] -> no;
                [A] -> A
            end
    end.

snaf_guesses(Total, Exp, Acc) ->
    One = math:pow(5, Exp),
    Two = One * 2,
    [
        to_snaf(Total - Two, Exp - 1, [$2|Acc]),
        to_snaf(Total - One, Exp - 1, [$1|Acc]),
        to_snaf(Total, Exp - 1, [$0|Acc]),
        to_snaf(Total + One, Exp - 1, [$-|Acc]),
        to_snaf(Total + Two, Exp - 1, [$=|Acc])
    ].

solve_2(_) ->
    ok.