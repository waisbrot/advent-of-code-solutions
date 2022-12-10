-module(aoc_problem_8).
-export([solve/2]).

solve(Problem, Example) ->
    Trees = aoc_timer:input(fun ingest/2, [Problem, Example]),
    aoc_timer:problem(1, fun solve_1/1, [Trees]),
    aoc_timer:problem(1, fun solve_2/1, [Trees]).

ingest(Problem, Example) ->
    Data = aoc_input:simple_read(Problem, Example),
    ingest_forest(Data, 0, 0, #{}).

ingest_forest(<<>>, MaxRow, _, #{cols := Cols} = Forest) ->
    {MaxRow+1, Cols, maps:delete(cols, Forest)};
ingest_forest(<<"\n", Data/binary>>, Row, MaxCol, Forest) ->
    ingest_forest(Data, Row+1, 0, Forest#{cols => MaxCol});
ingest_forest(<<Tree:1/bytes, Data/binary>>, Row, Col, Forest) ->
    Forest2 = Forest#{{Row,Col} => binary_to_integer(Tree)},
    ingest_forest(Data, Row, Col+1, Forest2).

% forest_to_strips(#{cols := Cols, rows := Rows} = Forest) ->
%     lists:map(fun (C) -> 
%         lists:map(fun (R) ->
%             maps:get({R, C}, Forest)
%         end, lists:seq(0, Rows-1))
%     end, lists:seq(0, Cols-1)).

solve_1({Cols, Rows, Forest}) ->
    Coordinates = queue:from_list(maps:keys(Forest)),
    Visibility = visibility_loop(Cols, Rows, Forest, Coordinates, maps:map(fun (_, _) -> unknown end, Forest)),
    io:format("Visibility=~p~n", [Visibility]).

visibility_loop(Cols, Rows, Forest, Coordinates, Visibility) ->
    case queue:out(Coordinates) of
        {empty, _} -> Visibility;
        {{value, Coord}, Coordinates2} ->
            {Coordinates2, Visibility2} = visibility_coordinates(Cols, Rows, Forest, Coord, Coordinates, Visibility),
            visibility_loop(Cols, Rows, Forest, Coordinates2, Visibility2)
    end.

visibility_coordinates(_Cols, _Rows, _Forest, {Row, Col} = Coord, Queue, Visibility) when Row =:= 0 orelse Col =:= 0 ->
    {Queue, Visibility#{Coord => visible}};
visibility_coordinates(Cols, Rows, _Forest, {Row, Col} = Coord, Queue, Visibility) when Row =:= Rows-1 orelse Col =:= Cols-1 ->
    {Queue, Visibility#{Coord => visible}};
visibility_coordinates(_C, _R, Forest, {Row, Col} = Coord, Queue, Visibility) ->
    Neighbors = neighbor_coordinates(Coord),
    %lists:map(fun (Neighbor) -> maps )
    ok.

neighbor_coordinates({X, Y}) ->
    [{X-1, Y}, {X+1, Y}, {X, Y-1}, {X, Y+1}].

solve_2(Forest) ->
    ok.

