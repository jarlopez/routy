-module(hist).
-author("Johan Mickos johanmi@kth.se").
-export([
         new/1
        ,update/3
        ]).

new(Node) ->
    [{Node, 0}].

update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of
        {Node, Age} ->
            if
                Age < N ->
                    new;
                true ->
                    old
            end;
        false ->
            not_found
    end.
