-module(aoc_input).
-export([
    simple_read/2
    ,split_by_blanks/2
    ,split_by_lines/2
]).

% Read a file
-spec simple_read(string, atom) -> binary.
simple_read(Name, Example) ->
    {Dir, Name2} = case Example of 
        full -> {"inputs", Name};
        e -> {"examples", Name};
        eb -> {"examples", Name ++ "b"}
    end,
    File = ["../", Dir, "/", Name2],
    %io:format("Read from ~p~n", [File]),
    {ok, Data} = file:read_file(File),
    Data.

split_by_blanks(Name, Example) ->
    RawData = simple_read(Name, Example),
    binary:split(RawData, <<"\n\n">>, [global]).

split_by_lines(Name, Example) ->
    RawData = simple_read(Name, Example),
    binary:split(RawData, <<"\n">>, [global]).
