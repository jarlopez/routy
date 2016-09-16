-module(interface).
-author("Johan Mickos johanmi@kth.se").
-export ([
          new/0
         ,add/4
         ,remove/2
         ,lookup/2
         ,ref/2
         ,name/2
         ,list/1
         ,broadcast/2
         ]).

new() ->
    ok.

add(Name, Ref, Pid, Intf) ->
    ok.

remove(Name, Intf) ->
    ok.

lookkup(Name, Intf) ->
    ok.

ref(Name, Intf) ->
    ok.

name(Name, Intf) ->
    ok.

list(Intf) ->
    ok.

broadcast(Msg, Intf) ->
    sok.