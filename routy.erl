-module(routy).

-export([
         start/2
        ,stop/1
        ,init/1
        ,router/6
        ,send_status/1
        ,bootstrap/0
        ,restore/1
        ,restore/2
        ,cleanup/0
        ]).

start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
    Node ! stop,
    unregister(Node).

restore(Node, Force) ->
    restore(Node, 'spion@2001:6b0:1:1041:e888:f9d6:6ce6:bf413', Force).

restore(Node) ->
    restore(Node, 'spion@2001:6b0:1:1041:e888:f9d6:6ce6:bf413', false).

start_all() ->
    start(boston, boston),
    start(new_york, new_york),
    start(austin, austin),
    start(san_francisco, san_francisco),
    start(reykjavik, reykjavik),
    start(nuuk, nuuk),
    start(trondheim, trondheim),
    start(oslo, oslo),
    start(gothemburg, gothemburg),
    start(stockholm, stockholm),
    start(lund, lund),
    start(copenhagen, copenhagen),
    start(berlin, berlin),
    start(paris, paris),
    start(madrid, madrid),
    start(helsinki, helsinki),
    start(rovaniemi, rovaniemi),
    start(moscow, moscow),

    ok.


restore(Node, Ip, Force) ->
    case Node of
        boston ->
            Connections = [austin, new_york, nuuk, reykjavik];
        new_york ->
            Connections = [austin, boston];
        austin ->
            Connections = [san_francisco, new_york, boston];
        san_francisco ->
            Connections = [austin];
        reykjavik ->
            Connections = [nuuk, boston, oslo, trondheim];
        nuuk ->
            Connections = [boston, reykjavik];
        trondheim ->
            Connections = [reykjavik, oslo, rovaniemi];
        oslo ->
            Connections = [reykjavik, trondheim, gothemburg, copenhagen];
        gothemburg ->
            Connections = [oslo, lund, stockholm];
        stockholm ->
            Connections = [gothemburg, lund, helsinki];
        lund ->
            Connections = [gothemburg, stockholm];
        copenhagen ->
            Connections = [oslo, berlin];
        berlin ->
            Connections = [copenhagen, paris, madrid, moscow];
        paris ->
            Connections = [madrid, berlin];
        madrid ->
            Connections = [paris, berlin];
        helsinki ->
            Connections = [rovaniemi, moscow, stockholm];
        rovaniemi ->
            Connections = [trondheim, helsinki];
        moscow ->
            Connections = [helsinki, berlin];
        _ ->
            Connections = []
    end,
    io:format("~p: ~p~n", [Node, Connections]),
    % case whereis(Node) of
    %     undefined ->
    %         ok;
    %     _Pid ->
    %         stop(Node)
    % end,
    % start(Node, Node),
    % Restart dependencies
    if
        Force == true ->
            start(Node, Node);
        true ->
            ok
    end,
    lists:map(fun(El) ->
        Node ! {add, El, {El, Ip}},
        if
            Force == true ->
                El ! {add, Node, {Node, Ip}};
            true ->
                ok
        end
    end, Connections).


bootstrap() ->
    Nodes = [
        boston,
        new_york,
        austin,
        san_francisco,
        reykjavik,
        oslo,
        trondheim,
        gothemburg,
        stockholm,
        nuuk,
        copenhagen,
        berlin,
        paris,
        madrid,
        helsinki,
        rovaniemi,
        moscow
    ],
    start_all(),
    lists:map(fun(El) -> restore(El) end, Nodes),

    % io:format("Creating nodes\t\tUSA~n"),
    % start(boston, boston),
    % start(new_york, new_york),
    % start(austin, austin),
    % start(san_francisco, san_francisco),

    % io:format("Creating nodes\t\tICELAND~n"),
    % start(reykjavik, reykjavik),

    % io:format("Creating nodes\t\tGREENLAND~n"),
    % start(nuuk, nuuk),

    % io:format("Creating nodes\t\tNORWAY~n"),
    % start(trondheim, trondheim),
    % start(oslo, oslo),

    % io:format("Creating nodes\t\tSWEDEN~n"),
    % start(gothemburg, gothemburg),
    % start(stockholm, stockholm),
    % start(lund, lund),

    % io:format("Creating nodes\t\tDENMARK~n"),
    % start(copenhagen, copenhagen),

    % io:format("Creating nodes\t\tGERMANY~n"),
    % start(berlin, berlin),

    % io:format("Creating nodes\t\tFRANCE~n"),
    % start(paris, paris),

    % io:format("Creating nodes\t\tSPAIN~n"),
    % start(madrid, madrid),

    % io:format("Creating nodes\t\tFINLAND~n"),
    % start(helsinki, helsinki),
    % start(rovaniemi, rovaniemi),

    % io:format("Creating nodes\t\tRUSSIA~n"),
    % start(moscow, moscow),

    % boston ! {add, new_york, {new_york, Ip}},
    % boston ! {add, austin, {austin, Ip}},
    % boston ! {add, reykjavik, {reykjavik, Ip}},
    % boston ! {add, nuuk, {nuuk, Ip}},

    % new_york ! {add, boston, {boston, Ip}},
    % new_york ! {add, austin, {austin, Ip}},

    % austin ! {add, san_francisco, {san_francisco, Ip}},
    % austin ! {add, new_york, {new_york, Ip}},
    % austin ! {add, boston, {boston, Ip}},

    % san_francisco ! {add, austin, {austin, Ip}},

    % nuuk ! {add, boston, {boston, Ip}},
    % nuuk ! {add, reykjavik, {reykjavik, Ip}},

    % reykjavik ! {add, boston, {boston, Ip}},
    % reykjavik ! {add, trondheim, {trondheim, Ip}},
    % reykjavik ! {add, oslo, {oslo, Ip}},
    % reykjavik ! {add, nuuk, {nuuk, Ip}},

    % trondheim ! {add, reykjavik, {reykjavik, Ip}},
    % trondheim ! {add, oslo, {oslo, Ip}},
    % trondheim ! {add, rovaniemi, {rovaniemi, Ip}},

    % oslo ! {add, trondheim, {trondheim, Ip}},
    % oslo ! {add, reykjavik, {reykjavik, Ip}},
    % oslo ! {add, gothemburg, {gothemburg, Ip}},
    % oslo ! {add, copenhagen, {copenhagen, Ip}},

    % copenhagen ! {add, oslo, {oslo, Ip}},
    % copenhagen ! {add, berlin, {berlin, Ip}},

    % berlin ! {add, paris, {paris, Ip}},
    % berlin ! {add, madrid, {madrid, Ip}},
    % berlin ! {add, copenhagen, {copenhagen, Ip}},

    % paris ! {add, madrid, {madrid, Ip}},
    % paris ! {add, berlin, {berlin, Ip}},

    % madrid ! {add, berlin, {berlin, Ip}},
    % madrid ! {add, paris, {paris, Ip}},

    % gothemburg ! {add, oslo, {oslo, Ip}},
    % gothemburg ! {add, lund, {lund, Ip}},
    % gothemburg ! {add, stockholm, {stockholm, Ip}},

    % lund ! {add, stockholm, {stockholm, Ip}},
    % lund ! {add, gothemburg, {gothemburg, Ip}},

    % stockholm ! {add, gothemburg, {gothemburg, Ip}},
    % stockholm ! {add, lund, {lund, Ip}},
    % stockholm ! {add, helsinki, {helsinki, Ip}},

    % helsinki ! {add, moscow, {moscow, Ip}},
    % helsinki ! {add, rovaniemi, {rovaniemi, Ip}},
    % helsinki ! {add, stockholm, {stockholm, Ip}},

    % rovaniemi ! {add, trondheim, {trondheim, Ip}},
    % rovaniemi ! {add, helsinki, {helsinki, Ip}},

    % moscow ! {add, helsinki, {helsinki, Ip}},

    ok.

cleanup() ->
    ok.

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
            io:format("[ADD @ ~p] ~p at ~p~n", [Name, Node, Pid]),
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
            io:format("~w: exit recived from ~w~n", [Name, Down]),
            Intf1 = intf:remove(Down, Intf),

            Map1 = map:update(Down, [], Map),
            Table1 = dijkstra:table(intf:list(Intf1), Map1),

            Hist1 = hist:drop(Down, Hist),

            % Update link-state
            Message = {links, Name, N, intf:list(Intf1)},
            intf:broadcast(Message, Intf1),


            % Update Map, Table
            router(Name, N+1, Hist1, Intf1, Table1, Map1);

%             % Update table with removed interface/node
%             Table1 = dijkstra:table(intf:list(Intf1), Map),

%             % Broadcast change to alert self, network
%             Message = {links, Name, N, intf:list(Intf1)},
%             intf:broadcast(Message, Intf1),

%             router(Name, N + 1, Hist, Intf1, Table1, Map);

% >>>>>>> Stashed changes
        {status, From} ->
            io:format("Received status from: ~p~n", [From]),
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);
        status ->
            io:format("[~p]~nN: ~p~nHist:~n~p~n~nIntf list: ~p~nTable:~n~p~n~nMap:~n~p~n~n", [Name, N, Hist, intf:list(Intf), Table, Map]),
            router(Name, N, Hist, Intf, Table, Map);
        {links, Node, R, Links} ->
            io:format("[links @ ~p] ~p - ~p - ~p ~n", [Name, Node, R, Links]),
            case hist:update(Node, R, Hist) of
                {new, Hist1} ->
                    intf:broadcast({links, Node, R, Links}, Intf),
                    Map1 = map:update(Node, Links, Map),
                    Table1 = dijkstra:table(intf:list(Intf), Map1),
                    router(Name, N, Hist1, Intf, Table1, Map1);
                old ->
                    router(Name, N, Hist, Intf, Table, Map)
            end;
        {route, Name, From, Message} ->
            io:format("~w: received message ~p from ~w~n", [Name, Message, From]),
            router(Name, N, Hist, Intf, Table, Map);
        {route, To, From, Message} ->
            io:format("~w: Routing from ~p to ~p~n", [Name, From, To]),
            case dijkstra:route(To, Table) of
                {ok, Gw} ->
                    case intf:lookup(Gw, Intf) of
                        {ok, Pid} ->
                            Pid ! {route, To, From, Message};
                        notfound ->
                            ok
                    end;
                notfound ->
                    io:format("Dropping packet from ~p to ~p~n", [From, To]),
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
% <<<<<<< Updated upstream
% =======
%             io:format("Broadcasting from ~p~n", [Name]),
% >>>>>>> Stashed changes
            Message = {links, Name, N, intf:list(Intf)},
            intf:broadcast(Message, Intf),
            router(Name, N+1, Hist, Intf, Table, Map);
        stop ->
            ok
    end.