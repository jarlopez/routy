-module(dijkstra).
-export ([
          entry/2
         ,replace/4
         ,update/4
         ]).

% Returns the length of the shortest path to the
% node or 0 if the node is not found.
entry(Node, Sorted) ->
    true.

% Replaces the entry for Node
% in Sorted with a new entry having a new length N and Gateway. The
% resulting list should of course be sorted.
%
replace(Node, N, Gateway, Sorted) ->
    true.

% Update the list Sorted given
% the information that Node can be reached in N hops using Gateway.
% If no entry is found then no new entry is added. Only if we have a
% better (shorter) path should we replace the existing entry.
update(Node, N, Gateway, Sorted) ->
    true.