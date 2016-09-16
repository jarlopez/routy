-module(test).
-export([print_case/3]).

print_case(CaseName, Result, Expected) ->
    io:format("\t~20s\t~p\t~p == ~p~n", [CaseName, Result == Expected, Expected, Result]).
