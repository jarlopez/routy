-module(map).
-export([
         new/0
        ,update/3
        ,reachable/2
        ,all_nodes/1
        ]).

new() ->
    [].

update(Node, Links, Map) ->
    lists:keystore(Node, 1, Map, {Node, Links}).

reachable(Node, Map) ->
    lists:keyfind(Node, 1, Map).

% Returns a list of _all_ nodes in the Map,
% even the ones without outgoing lists
all_nodes(Map) ->
    lists:usort(lists:foldl(fun({Node, Links}, Acc) ->
        [Node | Links ++ Acc]
    end,
    [], Map)).
