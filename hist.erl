-module(hist).
-author("Johan Mickos johanmi@kth.se").
-export([
         new/1
        ,update/3
        ]).

new(Node) ->
    [{Node, 0}].

%TODO Verify below functionality
update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of
        {Node, Age} ->
            if
                Age < N ->
                    {new, lists:keystore(Node, 1, History, {Node, N})};
                true ->
                    old
            end;
        false ->
            {new, lists:keystore(Node, 1, History, {Node, N})}
    end.
