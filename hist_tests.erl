-module(hist_tests).
-export([run/0]).

run() ->
    io:format("======================[~20s]======================~n", [?MODULE]),
    test_new(),
    test_update().

test_new() ->
    io:format("test_new()~n"),
    test:print_case("", hist:new(a), [{a, 0}]).

test_update() ->
    History = hist:new(a)
    io:format("test_update()~n"),
    test:print_case("Add to empty",
                    interface:add(a, b, c, []),
                    [{a, b, c}]),
    test:print_case("Add to existing",
                    interface:add(a, b, c, [{x, y, z}]),
                    [{a, b, c}, {x, y, z}]).
