-module(aoc_problem_7).
-export([solve/2]).

solve(Problem, Example) ->
    Files = aoc_timer:input(fun ingest/2, [Problem, Example]),
    %io:format("Files=~p~n", [Files]),
    aoc_timer:problem(1, fun solve_1/1, [Files]),
    aoc_timer:problem(2, fun solve_2/1, [Files]).

ingest(Problem, Example) ->
    Lines = aoc_input:split_by_lines(Problem, Example),
    ingest_lines(Lines, #{}, []).

ingest_lines([], Tree, _Cwd) ->
    Tree;
ingest_lines([<<"$ cd ", Path/binary>>|Lines], Tree, Cwd) ->
    ingest_cd(Path, Lines, Tree, Cwd);
ingest_lines([<<"$ ls">>|Lines], Tree, Cwd) ->
    ingest_ls(Lines, Tree, Cwd).

ingest_cd(<<"/">>, Lines, Tree, _Cwd) ->
    ingest_lines(Lines, Tree, []);
ingest_cd(<<"..">>, Lines, Tree, [_|Cwd]) ->
    ingest_lines(Lines, Tree, Cwd);
ingest_cd(Path, Lines, Tree, Cwd) ->
    ingest_lines(Lines, Tree, [Path|Cwd]).

ingest_ls([], Tree, _Cwd) ->
    Tree;
ingest_ls([<<"$",_/binary>>|_] = Lines, Tree, Cwd) ->  % backtrack!
    ingest_lines(Lines, Tree, Cwd);
ingest_ls([<<"dir ", Dir/binary>>|Lines], Tree, Cwd) ->
    Tree2 = add_directory(Dir, Tree, Cwd),
    ingest_ls(Lines, Tree2, Cwd);
ingest_ls([File|Lines], Tree, Cwd) ->
    [Size, Name] = binary:split(File, <<" ">>),
    Tree2 = add_file(Name, binary_to_integer(Size), Tree, Cwd),
    ingest_ls(Lines, Tree2, Cwd).

add_under_dir(Thing, Key, Tree, []) ->
    Tree#{Key => Thing};
add_under_dir(Thing, Key, Tree, Path) ->
    {Rest, [Cdir]} = lists:split(length(Path) - 1, Path),
    Subtree = maps:get(Cdir, Tree),
    Subtree2 = add_under_dir(Thing, Key, Subtree, Rest),
    Tree#{Cdir => Subtree2}.

add_directory(Dir, Tree, Cwd) ->
    add_under_dir(#{}, Dir, Tree, Cwd).

add_file(Name, Size, Tree, Cwd) ->
    add_under_dir(Size, Name, Tree, Cwd).

solve_1(Files) ->
    {_Total, DirectorySizes} = sum_tree(maps:iterator(Files), <<"/">>, #{}, 0),
    %io:format("DirectorySizes=~p~n", [DirectorySizes]),
    SmallerDirs = maps:filter(fun (_, Size) -> Size =< 100_000 end, DirectorySizes),
    lists:sum(maps:values(SmallerDirs)).

solve_2(Files) ->
    {Total, DirectorySizes} = sum_tree(maps:iterator(Files), <<"/">>, #{}, 0),
    Free = 70_000_000 - Total,
    Needed = 30_000_000 - Free,
    lists:min(maps:values(maps:filter(fun (_, Size) -> Size >= Needed end, DirectorySizes))).

sum_tree(Iter, Cwd, Sizes, CurSize) ->
    case maps:next(Iter) of
        none when not is_map_key(Cwd, Sizes) ->
            %io:format("End of dir ~p. Size ~p Sizes=~p~n", [Cwd, CurSize, Sizes]),
            {CurSize, Sizes#{Cwd => CurSize}};
        {FileName, Subtree, NextIter} when is_map(Subtree) ->
            {SubSize, Sizes2} = sum_tree(maps:iterator(Subtree), <<Cwd/binary, "/", FileName/binary>>, Sizes, 0),
            %io:format("Updated tree after ~p: ~p~n", [FileName, Sizes2]),
            CurSize2 = CurSize + SubSize,
            sum_tree(NextIter, Cwd, Sizes2, CurSize2);
        {_FileName, FileSize, NextIter} ->
            CurSize2 = CurSize + FileSize,
            sum_tree(NextIter, Cwd, Sizes, CurSize2)
    end.
