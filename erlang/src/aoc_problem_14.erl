-module(aoc_problem_14).
-export([solve/2]).

solve(Problem, Example) ->
    Cave = aoc_timer:input(fun ingest/2, [Problem, Example]),
    aoc_timer:problem(1, fun solve_1/1, [Cave]),
    % fprof:apply(aoc_timer, problem, [2, fun solve_2/1, [Monkeys]]).
    aoc_timer:problem(2, fun solve_2/1, [Cave]).

ingest(Problem, Example) ->
    LineCommands = aoc_input:split_by_lines(Problem, Example),
    lists:foldl(fun 
        (Line, start) ->
            parse_line(Line);
        (Line, {{Rmax, Rmin, Dmax, Dmin}, Map}) ->
            {{Rmax1, Rmin1, Dmax1, Dmin1}, Map1} = parse_line(Line),
            MinMax = {max(Rmax1, Rmax), min(Rmin1, Rmin), max(Dmax1, Dmax), min(Dmin1, Dmin)},
            {MinMax, sets:union(Map, Map1)}
    end, start, LineCommands).

parse_line(LineStr) ->
    SegmentsStr = binary:split(LineStr, <<" -> ">>, [global]),
    [{R0,D0} = Segment|Segments] = lists:map(fun parse_segment/1, SegmentsStr),
    Map = parse_line(Segment, Segments, sets:from_list([Segment])),
    MinMax = lists:foldl(fun ({R, D}, {Rmax, Rmin, Dmax, Dmin}) ->
        {max(R, Rmax), min(R, Rmin), max(D, Dmax), min(D, Dmin)}
    end, {R0, R0, D0, D0}, sets:to_list(Map)),
    {MinMax, Map}.

parse_line(_, [], Map) ->
    Map;
parse_line(Curr, [Curr|Segments], Map) ->
    parse_line(Curr, Segments, sets:add_element(Curr, Map));
parse_line({R,D1}, [{R,D2}=Next|Segments], Map) ->
    Map2 = lists:foldl(fun (D, MapN) -> 
        sets:add_element({R, D}, MapN)
    end, Map, lists:seq(min(D1,D2), max(D1, D2))),
    parse_line(Next, Segments, Map2);
parse_line({R1, D}, [{R2, D}=Next|Segments], Map) ->
    Map2 = lists:foldl(fun (R, MapN) ->
        sets:add_element({R, D}, MapN)
    end, Map, lists:seq(min(R1, R2), max(R1, R2))),
    parse_line(Next, Segments, Map2).

parse_segment(Binary) ->
    [R,D] = binary:split(Binary, <<",">>),
    {binary_to_integer(R), binary_to_integer(D)}.

solve_1(Map) ->
    % draw_map(Map),
    count_drops_to_abyss(0, Map).

draw_map({{_, Rmin, _, Dmin}=MinMax, Map}) ->
    draw_map({Rmin, Dmin}, MinMax, Map).

draw_map({Rmax, Dmax}=Curr, {Rmax, _, Dmax, _}, Map) ->
    draw_pixel(Curr, Map),
    io:format("~n");
draw_map({Rmax, D0}=Curr, {Rmax, Rmin, _, _}=MinMax, Map) ->
    draw_pixel(Curr, Map),
    io:format("~n"),
    draw_map({Rmin, D0+1}, MinMax, Map);
draw_map({R0, D0}=Curr, MinMax, Map) ->
    draw_pixel(Curr, Map),
    draw_map({R0+1, D0}, MinMax, Map).

draw_pixel(P, Map) ->
    case sets:is_element(P, Map) of
        true -> io:format("#");
        false -> io:format(".")
    end.

count_drops_to_abyss(N, Map) ->
    case drop_one({500, 0}, Map) of
        Map -> N;
        Map2 -> count_drops_to_abyss(N+1, Map2)
    end.

drop_one({R,D}, {{Rmax, Rmin, Dmax, _}, _}=Map) when R > Rmax; R < Rmin; D > Dmax ->
    Map;
drop_one({R,D}, {MinMax, Map}=AllMap) ->
    case sets:is_element({R, D+1}, Map) of
        false -> drop_one({R,D+1}, AllMap);
        true -> case sets:is_element({R-1, D+1}, Map) of
            false -> drop_one({R-1, D+1}, AllMap);
            true -> case sets:is_element({R+1, D+1}, Map) of
                false -> drop_one({R+1, D+1}, AllMap);
                true -> {MinMax, sets:add_element({R,D}, Map)}
            end
        end
    end.

solve_2(Map) ->
    count_drops_to_fill(0, Map).

count_drops_to_fill(N, {{_, _, Dmax, _}, Map}) ->
    count_drops_to_fill(N, Dmax+2, Map).
count_drops_to_fill(N, Floor, Map) ->
    case sets:is_element({500, 0}, Map) of
        true -> 
            N;
        false ->
            Map2 = drop_one({500, 0}, Floor, Map),
            count_drops_to_fill(N+1, Floor, Map2)
    end.

drop_one({R, D}, Floor, Map) when D+1 =:= Floor ->
    sets:add_element({R,D}, Map);
drop_one({R, D}, Floor, Map) ->
    case sets:is_element({R, D+1}, Map) of
        false -> drop_one({R,D+1}, Floor, Map);
        true -> case sets:is_element({R-1, D+1}, Map) of
            false -> drop_one({R-1, D+1}, Floor, Map);
            true -> case sets:is_element({R+1, D+1}, Map) of
                false -> drop_one({R+1, D+1}, Floor, Map);
                true -> sets:add_element({R,D}, Map)
            end
        end
    end.
