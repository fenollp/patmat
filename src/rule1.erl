%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(rule1).
-compile(nowarn_unused_vars).

%% rule: rule1.

-export([ doc/0
        , r/1
        , ast/0 ]).


-define(ignore_anything_else
       , eof -> ok;
         _M  ->
               %% io:format("Skipped ~p\n", [_M]),
               r(Parent, Seen, Closure)).

-record(c, {x='', y='', z=''}).

-define(P1(X,Y), {X, on, Y}).       %% X Y
-define(P2(Y,Z), {Y, left_of, Z}).  %%   Y Z
-define(P3(Z)  , {Z, color, red}).  %%     Z
-define(R(X,Z), {Z, on, X}).

%% API

doc () ->
    "(defrule find-stack-of-two-blocks-to-the-left-of-a-red-block"
        "    (<x> ^on <y>)"
        "    (<y> ^left-of <z>)"
        "    (<z> ^color red)"
        " => ( <z> ^on <x> ) )".

ast () ->
    {rule, rule1, [ {{id,x}, on, {id,y}}
                  , {{id,y}, left_of, {id,z}}
                  , {{id,z}, color, red}
                  ]
    , {{id,z}, on, {id,x}}}.

r (Parent) ->
    r(Parent, [], #c{}).

r (Parent, Seen, Closure=#c{})
  when Seen == [] ->
    receive
        ?P1(X,Y) -> r(Parent, [1|Seen], Closure#c{x=X, y=Y});
        ?P2(Y,Z) -> r(Parent, [2|Seen], Closure#c{y=Y, z=Z});
        ?P3(Z)   -> r(Parent, [3|Seen], Closure#c{z=Z});
        ?ignore_anything_else
    end;

r (Parent, Seen, Closure=#c{x=X, y=Y})
  when Seen == [1] ->
    receive
        ?P2(Y,Z) -> r(Parent, [2|Seen], Closure#c{z=Z});
        ?P3(Z)   -> r(Parent, [3|Seen], Closure#c{z=Z});
        ?ignore_anything_else
    end;

r (Parent, Seen, Closure=#c{y=Y, z=Z})
  when Seen == [2] ->
    receive
        ?P1(X,Y) -> r(Parent, [1|Seen], Closure#c{x=X});
        ?P3(Z)   -> r(Parent, [3|Seen], Closure);
        ?ignore_anything_else
    end;

r (Parent, Seen, Closure=#c{z=Z})
  when Seen == [3] ->
    receive
        ?P1(X,Y) -> r(Parent, [1|Seen], Closure#c{x=X, y=Y});
        ?P2(Y,Z) -> r(Parent, [2|Seen], Closure#c{y=Y});
        ?ignore_anything_else
    end;

r (Parent, Seen, Closure=#c{x=X, y=Y, z=Z})
  when Seen == [1,2] orelse Seen == [2,1] ->
    receive
        ?P3(Z)   -> r(Parent, [3|Seen], Closure);
        ?ignore_anything_else
    end;

r (Parent, Seen, Closure=#c{x=X, y=Y, z=Z})
  when Seen == [1,3] orelse Seen == [3,1] ->
    receive
        ?P2(Y,Z) -> r(Parent, [2|Seen], Closure);
        ?ignore_anything_else
    end;

r (Parent, Seen, Closure=#c{y=Y, z=Z})
  when Seen == [2,3] orelse Seen == [3,2] ->
    receive
        ?P1(X,Y) -> r(Parent, [1|Seen], Closure#c{x=X});
        ?ignore_anything_else
    end;

r (Parent, Seen, #c{x=X, y=Y, z=Z}) ->
%%  when length(Seen) == 3 ->
    patmat:p(?R(X,Z), Parent),
    Bindings = [{x,X}, {y,Y}, {z,Z}],
    io:format("Seen = ~p | ~p\n", [Seen,Bindings]).%%

%% End of Module.
