-module(routy).

-export([
         start/2
        ,stop/1
        ,init/1
        ,router/6
        ,send_status/1
        ]).

start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
    Node ! stop,
    unregister(Node).

init(Name) ->
    Intf = intf:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    router(Name, 0, Hist, Intf, Table, Map).

send_status(Pid) ->
    Pid ! {status, self()},
    receive
        Msg ->
            io:format("[STATUS: ~p] ~p", [Pid, Msg])
    end.

router(Name, N, Hist, Intf, Table, Map) ->
    io:format("Running as ~p", [Name]),
    receive
        {add, Node, Pid} ->
            io:format("[ADD @ ~p] ~p at ~p", [Name, Node, Pid]),
            Ref = erlang:monitor(process,Pid),
            Intf1 = intf:add(Node, Ref, Pid, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {remove, Node} ->
            {ok, Ref} = intf:ref(Node, Intf),
            erlang:demonitor(Ref),
            Intf1 = intf:remove(Node, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {'DOWN', Ref, process, _, _} ->
            {ok, Down} = intf:name(Ref, Intf),
            io:format("~w: exit recived from ~w~n", [Name, Down]),
            Intf1 = intf:remove(Down, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {status, From} ->
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);
        stop ->
            ok
    end.