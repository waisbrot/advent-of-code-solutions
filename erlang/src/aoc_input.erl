-module(aoc_input).
-export([
    simple_read/2
    ,split_by_blanks/2
]).

% Read a file
-spec simple_read(string, bool) -> binary.
simple_read(Name, Example) ->
    Dir = case Example of 
        true -> "examples";
        false -> "inputs"
    end,
    File = ["../", Dir, "/", Name],
    %io:format("Read from ~p~n", [File]),
    {ok, Data} = file:read_file(File),
    Data.

split_by_blanks(Name, Example) ->
    RawData = simple_read(Name, Example),
    binary:split(RawData, <<"\n\n">>, [global]).