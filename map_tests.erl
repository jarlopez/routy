-module(map_tests).
-export([run/0]).

run() ->
    test_new(),
    test_update(),
    test_reachable(),
    test_all_nodes().

print_case(CaseName, Result, Expected) ->
    io:format("\tCase '~s'\t\t~p\t~p == ~p~n", [CaseName, Result == Expected, Expected, Result]).

test_new() ->
    io:format("test_new()~n"),
    print_case("create new", map:new(), []).

test_update() ->
    io:format("test_update()~n"),
    print_case("update empty", map:update(berlin, [london, paris], []), [{berlin,[london,paris]}]),
    print_case("update existing", map:update(berlin, [madrid], [{berlin,[london,paris]}]), [{berlin, [madrid]}]).

test_reachable() ->
    MultiEntryMap = [
        {london, []},
        {berlin, [london]},
        {stockholm, [london, berlin, helsinki]}
    ],

    io:format("test_reachable()~n"),
    print_case("not reachable", map:reachable(some_node, []), false),
    print_case("reachable with links", map:reachable(stockholm, MultiEntryMap), {stockholm, [london, berlin, helsinki]}),
    print_case("reachable w/o links", map:reachable(london, MultiEntryMap), {london, []}).

test_all_nodes() ->
    SingleEntryMap = [{london, []}],
    MultiEntryMap = [
        {london, []},
        {berlin, [london]},
        {stockholm, [london, berlin, helsinki]}
    ],
    MultiEntryNodes = [berlin, helsinki, london, stockholm],
    io:format("test_all_nodes()~n"),
    print_case("empty map", map:all_nodes([]), []),
    print_case("single entry", map:all_nodes(SingleEntryMap), [london]),
    print_case("mutliple entries", map:all_nodes(MultiEntryMap), MultiEntryNodes).