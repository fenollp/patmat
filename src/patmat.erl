%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(patmat).

%% patmat: 

-export([ facts/0
        , main/0
        ]).


%% API

facts () ->
    [ {b3, color, red}
    , {b1, on, b2}
    ] ++ lists:duplicate(200*1000, {b1, on, b3}) ++
    [ {b1, color, red}
    , {b2, on, table}
    , {b2, left_of, b3}
    , eof ].

main () ->
    S = self(),
    lists:foreach(
      fun (Module) ->
              io:format("\t~p\n", [Module]),
              R = spawn(Module, r, [S]),
              timer:sleep(500),
              [R ! Fact || Fact <- facts()],
              receive
                  M -> io:format("-> ~p\n", [M])
              after 1000 ->
                      io:format("-> .\n", [])
              end,
              io:format("\n", [])
      end, [rule1]).

%% Internals

%% perms ([]) -> [[]];
%% perms (L) ->
%%     [[H|T] || H <- L,
%%               T <- perms(L -- [H])].

%% End of Module.
