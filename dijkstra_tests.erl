-module(dijkstra_tests).
-export([run/0]).

run() ->
    test_entry(),
    test_replace(),
    test_update(),
    test_iterate().

test_entry() ->
    io:format("test_entry()~n"),
    test:print_case("Single lookup", dijkstra:entry(berlin, [{berlin, 2, paris}]), 2).

test_replace() ->
    io:format("test_replace()~n"),
    test:print_case("Single replace", dijkstra:replace(stockholm, 1, uppsala, [
        {berlin, 1, paris},
        {stockholm, 2, helsinki},
        {beta, 3, theta}
    ]), [
        {stockholm, 1, uppsala},
        {berlin, 1, paris},
        {beta, 3, theta}
    ]).

test_update() ->
    SortedList = [
        {berlin, 1, paris},
        {stockholm, 2, helsinki},
        {beta, 3, theta}
    ],
    UpdatedList = [
        {beta, 1, theta},
        {berlin, 1, paris},
        {stockholm, 2, helsinki}
    ],
    io:format("test_update()~n"),
    test:print_case("No existing path", dijkstra:update(new_york, 1, boston, SortedList), SortedList),
    test:print_case("New path not shorter", dijkstra:update(berlin, 3, paris, SortedList), SortedList),
    test:print_case("Shorter new path", dijkstra:update(beta, 1, theta, SortedList), UpdatedList),
    test:print_case("Empty list", dijkstra:update(london, 2, amsterdam, []), []),
    test:print_case("Update & change", dijkstra:update(london, 1, stockholm, [{berlin, 2, paris}, {london, 3, paris}]), [{london,1,stockholm}, {berlin, 2, paris}]).

test_iterate() ->
    io:format("test_iterate()~n"),
    test:print_case("",
    dijkstra:iterate([{paris, 0, paris}, {berlin, inf, unknown}], [{paris, [berlin]}], []), [{paris, paris},{berlin,paris}]).