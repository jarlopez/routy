-module(sim).
-export([
         restore/1
        ,restore/2
        ,start_all/0
        ,bs/0
        ,bootstrap/0
        ]).

restore(Node, Force) ->
    restore(Node, 'spion@2001:6b0:1:1041:e888:f9d6:6ce6:bf413', Force).

restore(Node) ->
    restore(Node, 'spion@2001:6b0:1:1041:e888:f9d6:6ce6:bf413', true).

start_all() ->
    routy:start(boston, boston),
    routy:start(new_york, new_york),
    routy:start(austin, austin),
    routy:start(san_francisco, san_francisco),
    routy:start(reykjavik, reykjavik),
    routy:start(nuuk, nuuk),
    routy:start(trondheim, trondheim),
    routy:start(oslo, oslo),
    routy:start(gothemburg, gothemburg),
    routy:start(stockholm, stockholm),
    routy:start(lund, lund),
    routy:start(copenhagen, copenhagen),
    routy:start(berlin, berlin),
    routy:start(paris, paris),
    routy:start(madrid, madrid),
    routy:start(helsinki, helsinki),
    routy:start(rovaniemi, rovaniemi),
    routy:start(moscow, moscow),
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
            Connections = [boston, reykjavik, trondheim];
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
    if
        Force == true ->
            routy:start(Node, Node);
        true ->
            ok
    end,
    lists:map(fun(El) ->
        Node ! {add, El, {El, Ip}},
        if
            Force == true ->
                El ! {add, Node, {Node, Ip}},
                ok;
            true ->
                ok
        end
    end, Connections),
    ok.

bs() ->
    bootstrap().

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
    lists:map(fun(El) -> restore(El, false) end, Nodes),
    ok.