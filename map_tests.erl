-module(map_tests).
-export([run/0]).

run() ->
    io:format("======================[~20s]======================~n", [?MODULE]),
    test_new(),
    test_update(),
    test_reachable(),
    test_all_nodes().

test_new() ->
    io:format("test_new()~n"),
    test:print_case("create new", map:new(), []).

test_update() ->
    io:format("test_update()~n"),
    test:print_case("update empty", map:update(berlin, [london, paris], []), [{berlin,[london,paris]}]),
    test:print_case("update existing", map:update(berlin, [madrid], [{berlin,[london,paris]}]), [{berlin, [madrid]}]).

test_reachable() ->
    MultiEntryMap = [
        {london, []},
        {berlin, [london]},
        {stockholm, [london, berlin, helsinki]}
    ],

    io:format("test_reachable()~n"),
    test:print_case("not reachable", map:reachable(some_node, []), []),
    test:print_case("reachable with links", map:reachable(stockholm, MultiEntryMap), [london, berlin, helsinki]),
    test:print_case("reachable w/o links", map:reachable(london, MultiEntryMap), []).

test_all_nodes() ->
    SingleEntryMap = [{london, []}],
    MultiEntryMap = [
        {london, []},
        {berlin, [london]},
        {stockholm, [london, berlin, helsinki]}
    ],
    MultiEntryNodes = [berlin, helsinki, london, stockholm],
    io:format("test_all_nodes()~n"),
    test:print_case("empty map", map:all_nodes([]), []),
    test:print_case("single entry", map:all_nodes(SingleEntryMap), [london]),
    test:print_case("mutliple entries", map:all_nodes(MultiEntryMap), MultiEntryNodes).