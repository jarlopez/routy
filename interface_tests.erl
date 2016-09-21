-module(interface_tests).
-export([run/0]).

run() ->
    io:format("======================[~20s]======================~n", [?MODULE]),
    test_new(),
    test_add(),
    test_remove(),
    test_lookup(),
    test_ref(),
    test_name(),
    test_list(),
    test_broadcast().

test_new() ->
    io:format("test_new()~n"),
    test:print_case("", interface:new(), []).

test_add() ->
    io:format("test_add()~n"),
    test:print_case("Add to empty",
                    interface:add(a, b, c, []),
                    [{a, b, c}]),
    test:print_case("Add to existing",
                    interface:add(a, b, c, [{x, y, z}]),
                    [{a, b, c}, {x, y, z}]).

test_remove() ->
    io:format("test_remove()~n"),
    test:print_case("", interface:remove(a, [{a, b, c}, {x, y, z}]),
                    [{x, y, z}]).

test_lookup() ->
    test:print_case("Lookup existing",
                    interface:lookup(a, [{x, y, z}, {a, b, c}]),
                    {ok, c}),
    test:print_case("Lookup not found",
                    interface:lookup(foo, [{x, y, z}, {a, b, c}]),
                    notfound).

test_ref() ->
    io:format("test_ref()~n"),
    test:print_case("Lookup existing",
                    interface:ref(a, [{x, y, z}, {a, b, c}]),
                    {ok, b}),
    test:print_case("Lookup not found",
                    interface:ref(foo, [{x, y, z}, {a, b, c}]),
                    notfound).

test_name() ->
    io:format("test_name()~n"),
    test:print_case("Lookup existing",
                    interface:name(b, [{x, y, z}, {a, b, c}]),
                    {ok, a}),
    test:print_case("Lookup not found",
                    interface:name(foo, [{x, y, z}, {a, b, c}]),
                    notfound).

test_list() ->
    io:format("test_list()~n"),
    test:print_case("",
                    interface:list([{x, y, z}, {a, b, c}]),
                    [x, a]).

test_broadcast() ->
    io:format("test_list()~n"),
    % TODO
    test:print_case("", true, true).
