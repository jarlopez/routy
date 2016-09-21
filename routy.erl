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
            io:format("[STATUS: ~p] ~p~n", [Pid, Msg])
    end.

router(Name, N, Hist, Intf, Table, Map) ->
    receive
        {add, Node, Pid} ->
            io:format("[~s]\t~-13w~13w\t~w~n", [color:cyan("add"), Name, Node, Pid]),
            Ref = erlang:monitor(process,Pid),
            Intf1 = intf:add(Node, Ref, Pid, Intf),
            Message = {links, Name, N, intf:list(Intf1)},
            intf:broadcast(Message, Intf1),
            router(Name, N+1, Hist, Intf1, Table, Map);

        {remove, Node} ->
            {ok, Ref} = intf:ref(Node, Intf),
            erlang:demonitor(Ref),
            Intf1 = intf:remove(Node, Intf),
            router(Name, N, Hist, Intf1, Table, Map);

        {'DOWN', Ref, process, _, _} ->
            {ok, Down} = intf:name(Ref, Intf),
            io:format("[~s]\t~-13w\tExit recived from ~w~n", [color:redb("down"), Name, Down]),
            Intf1 = intf:remove(Down, Intf),

            Map1 = map:update(Down, [], Map),
            Table1 = dijkstra:table(intf:list(Intf1), Map1),

            Hist1 = hist:drop(Down, Hist),

            % Update link-state
            Message = {links, Name, N, intf:list(Intf1)},
            intf:broadcast(Message, Intf1),

            router(Name, N+1, Hist1, Intf1, Table1, Map1);

        {status, From} ->
            io:format("Received status from: ~p~n", [From]),
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);

        status ->
            io:format("[~p]~nN: ~p~nHist:~n~p~n~nIntf list: ~p~nTable:~n~p~n~nMap:~n~p~n~n", [Name, N, Hist, intf:list(Intf), Table, Map]),
            router(Name, N, Hist, Intf, Table, Map);

        {links, Node, R, Links} ->
            case hist:update(Node, R, Hist) of
                {new, Hist1} ->
                    io:format("[~s]\t~-13w~13w\tN=~w\t~w ~n", [color:magenta("link"), Name, Node, R, Links]),
                    intf:broadcast({links, Node, R, Links}, Intf),
                    Map1 = map:update(Node, Links, Map),
                    Table1 = dijkstra:table(intf:list(Intf), Map1),
                    router(Name, N, Hist1, Intf, Table1, Map1);
                old ->
                    router(Name, N, Hist, Intf, Table, Map)
            end;

        {route, Name, From, Message} ->
            io:format("[~s]\t~-13s Received message ~s from ~s~n", [color:greenb("route"), Name, color:greenb(Message), From]),
            router(Name, N, Hist, Intf, Table, Map);
        {route, To, From, Message} ->
            io:format("[~s]\t~-13w Routing from ~p to ~p~n", [color:yellow("route"), Name, From, To]),
            case dijkstra:route(To, Table) of
                {ok, Gw} ->
                    case intf:lookup(Gw, Intf) of
                        {ok, Pid} ->
                            Pid ! {route, To, From, Message};
                        notfound ->
                            ok
                    end;
                notfound ->
                    io:format("[~s]\tDropping packet from ~p to ~p~n", [color:redb("drop"), From, To]),
                    ok % :(
            end,
            router(Name, N, Hist, Intf, Table, Map);

        {send, To, Message} ->
            self() ! {route, To, Name, Message},
            router(Name, N, Hist, Intf, Table, Map);

        update ->
            Table1 = dijkstra:table(intf:list(Intf), Map),
            router(Name, N, Hist, Intf, Table1, Map);
        broadcast ->
            Message = {links, Name, N, intf:list(Intf)},
            intf:broadcast(Message, Intf),
            router(Name, N+1, Hist, Intf, Table, Map);
        stop ->
            ok
    end.