%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(rule1).
-compile(nowarn_unused_vars).

%% rule1: 

-export([ doc/0
        , r/1
        , ast/0 ]).


-record(abc, {x='', y='', z=''}).

-define(A(X,Y), {X, on, Y}).       %% X Y
-define(B(Y,Z), {Y, left_of, Z}).  %%   Y Z
-define(C(Z)  , {Z, color, red}).  %%     Z
-define(R(X,Z), {Z, on, X}).

-define(ignore_anything_else
       , eof -> ok;
         _M  ->
               %% io:format("Skipped ~p\n", [_M]),
               r(Parent, Seen, Closure)).

%% API

doc () ->
    "(defrule find-stack-of-two-blocks-to-the-left-of-a-red-block"
        "    (<x> ^on <y>)"
        "    (<y> ^left-of <z>)"
        "    (<z> ^color red)"
        " => ( <z> ^on <x> ) )".

ast () ->
    {rule, [ {{id,x}, on, {id,y}}
           , {{id,y}, left_of, {id,z}}
           , {{id,z}, color, red}
           ], {{id,z}, on, {id,x}}}.

r (Parent) -> r(Parent, [], '').
p (R, Pid) ->
    Pid ! R,
    Info0 = process_info(self()),
    Info  = lists:keydelete(messages, 1, Info0),
    io:format("<- ~p\n", [Info]).

r (Parent, Seen=[], Closure) ->
    receive
        ?A(X,Y) -> r(Parent, [a], #abc{x=X, y=Y});
        ?B(Y,Z) -> r(Parent, [b], #abc{y=Y, z=Z});
        ?C(Z)   -> r(Parent, [c], #abc{z=Z});
        ?ignore_anything_else
    end;

r (Parent, Seen=[a], Closure=#abc{x=X, y=Y}) ->
    receive
        ?B(Y,Z) -> r(Parent, Seen++[b], Closure#abc{z=Z});
        ?C(Z)   -> r(Parent, Seen++[c], Closure#abc{z=Z});
        ?ignore_anything_else
    end;

r (Parent, Seen=[b], Closure=#abc{y=Y, z=Z}) ->
    receive
        ?A(X,Y) -> r(Parent, Seen++[a], Closure#abc{x=X});
        ?C(Z)   -> r(Parent, Seen++[c], Closure);
        ?ignore_anything_else
    end;

r (Parent, Seen=[c], Closure=#abc{z=Z}) ->
    receive
        ?A(X,Y) -> r(Parent, Seen++[a], Closure#abc{x=X, y=Y});
        ?B(Y,Z) -> r(Parent, Seen++[b], Closure#abc{y=Y});
        ?ignore_anything_else
    end;

r (Parent, Seen, Closure=#abc{x=X, y=Y, z=Z})
  when Seen == [a,b] orelse Seen == [b,a] ->
    receive
        ?C(Z)   -> r(Parent, Seen++[c], Closure);
        ?ignore_anything_else
    end;

r (Parent, Seen, Closure=#abc{x=X, y=Y, z=Z})
  when Seen == [a,c] orelse Seen == [c,a] ->
    receive
        ?B(Y,Z) -> r(Parent, Seen++[b], Closure);
        ?ignore_anything_else
    end;

r (Parent, Seen, Closure=#abc{y=Y, z=Z})
  when Seen == [b,c] orelse Seen == [c,b] ->
    receive
        ?A(X,Y) -> r(Parent, Seen++[a], Closure#abc{x=X});
        ?ignore_anything_else
    end;

r (Parent, Seen, #abc{x=X, y=Y, z=Z})
  when length(Seen) == 3 ->
    io:format("Seen = ~p\n", [Seen]),%%
    p(?R(X,Z), Parent).

%% End of Module.
