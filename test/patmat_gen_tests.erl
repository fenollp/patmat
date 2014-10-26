%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(patmat_gen_tests).

%% patmat_gen_tests: tests for module patmat_gen.

-include_lib("eunit/include/eunit.hrl").

-define(print(Fmt,Data)
       , io:format(user, Fmt, Data)).

%% API tests.

rule1_test () ->
    Facts = [ {b3, color, red}
            , {b1, on, b2}
            , {b1, on, b3}
            , {b1, color, red}
            , {b2, on, table}
            , {b2, left_of, b3}
            , eof ],
    Name = rule1,
    Rule = {rule, Name, [ {{id,x}, on, {id,y}}
                        , {{id,y}, left_of, {id,z}}
                        , {{id,z}, color, red}
                        ]
           , {{id,z}, on, {id,x}}},
    File = patmat_gen:file("ebin", Rule),
    {ok,_} = compile:file(File, [{outdir, "ebin"}]),
    ?print("\t~p\n", [Name]),
    R = spawn(Name, r, [self()]),
    timer:sleep(50),
    [R ! Fact || Fact <- Facts],
    Ret = receive  M -> ?print("-> ~p\n", [M]), M
          after 1000 -> ?print("->  .\n", [])
          end,
    ?assertEqual({b3,on,b1}, Ret).

%% Internals

%% End of Module.
