-module(test).
-export([print_case/3]).

print_case(CaseName, Result, Expected) ->
    io:format("\tCase '~s'\t\t~p\t~p == ~p~n", [CaseName, Result == Expected, Expected, Result]).
