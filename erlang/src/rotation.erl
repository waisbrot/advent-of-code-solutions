-module(rotation).
-export([
    from_list/1,
    to_list/1,
    next/1
]).

from_list(List) ->
    queue:from_list(List).

to_list(R) ->
    queue:to_list(R).

next(R) ->
    {{value, N}, R2} = queue:out(R),
    {N, queue:in(N, R2)}.
