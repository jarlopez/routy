-module(dijkstra).
-author("Johan Mickos johanmi@kth.se").
-export ([
         %TODO Only export table/2 and route/2
          entry/2
         ,replace/4
         ,update/4
         ,iterate/3
         ,table/2
         ,route/2
         ]).

% Returns the length of the shortest path to the
% node or 0 if the node is not found.
entry(Node, Sorted) ->
    El = lists:keyfind(Node, 1, Sorted),
    case El of
        {_Node, Distance, _Gateway} ->
            Distance;
        _ ->
            0
    end.

% Replaces the entry for Node
% in Sorted with a new entry having a new length N and Gateway. The
% resulting list should of course be sorted.
replace(Node, N, Gateway, Sorted) ->
    lists:sort(fun(A, B) -> sorting_helper(A, B) end, lists:keystore(Node, 1, Sorted, {Node, N, Gateway})).

sorting_helper({_, DistA, _}, {_, DistB, _}) ->
    DistA < DistB.

% Update the list Sorted given
% the information that Node can be reached in N hops using Gateway.
% If no entry is found then no new entry is added. Only if we have a
% better (shorter) path should we replace the existing entry.
update(Node, N, Gateway, Sorted) ->
    case entry(Node, Sorted) of
        Dist when N < Dist ->
            replace(Node, N, Gateway, Sorted);
        _Dist ->
            Sorted
    end.

% Construct a table given a sorted list
% of nodes, a map and a table constructed so far.
iterate([], _, Table) ->
    Table;
iterate([{ _, inf, _ } | _], _, Table) ->
    Table;
iterate([{Node, N, Gateway} | T], Map, Table) ->
    Reachable = map:reachable(Node, Map),
    NewList = lists:foldl(fun(El, Acc) ->
        Dist = N + 1,
        update(El, Dist, Gateway, Acc)
    end, T, Reachable),
    iterate(NewList, Map, [ {Node, Gateway} | Table]).

table(Gateways, Map) ->
    All = map:all_nodes(Map),
    Initial = lists:map(fun(El) ->
        {El, inf, unknown}
    end, All),
    Nodes = lists:foldl(fun(El, Acc) ->
            update(El, 0, El, Acc)
        end, Initial, Gateways),
    iterate(Nodes, Map, []).

route(Node, Table) ->
    io:format("Routing to ~p with\n~p\n", [Node, Table]),
    case lists:keyfind(Node, 1, Table) of
        {Node, Gateway} ->
            {ok, Gateway};
        false ->
            not_found
    end.
