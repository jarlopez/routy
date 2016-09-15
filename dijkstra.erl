-module(dijkstra).
-export ([
          entry/2
         ,replace/4
         ,update/4
         ]).

% Returns the length of the shortest path to the
% node or 0 if the node is not found.
entry(Node, Sorted) ->
    El = lists:keyfind(Node, 1, Sorted),
    case El of
        {Node, Distance, Gateway} ->
            Distance;
        _ ->
            0
    end.

% Replaces the entry for Node
% in Sorted with a new entry having a new length N and Gateway. The
% resulting list should of course be sorted.
replace(Node, N, Gateway, Sorted) ->
    lists:sort(fun(A, B) -> sorting_helper(A, B) end, lists:keystore(Node, 1, Sorted, {Node, N, Gateway})).

% Update the list Sorted given
% the information that Node can be reached in N hops using Gateway.
% If no entry is found then no new entry is added. Only if we have a
% better (shorter) path should we replace the existing entry.
update(Node, N, Gateway, Sorted) ->
    Res = entry(Node, Sorted),
    case Res of
        0 ->
            % Path does not exist, do nothing and return Sorted
            % io:format("Path doesnt exist~n"),
            Sorted;
        Res when N < Res->
            % io:format("Updating because path ~p shorter than old ~p~n", [N, Res]),
            replace(Node, N, Gateway, Sorted);
        _ ->
            % io:format("NOT updating because path ~p shorter than old ~p~n", [N, Res]),
            Sorted
    end.

% Helper functions

sorting_helper({_, DistA, _}, {_, DistB, _}) ->
    DistA < DistB.
