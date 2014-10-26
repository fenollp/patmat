%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(patmat).

%% patmat: 

-export([ facts/0
        , doc/1
        ]).


%% API

facts () ->
    [ {b3, color, red}
    , {b1, on, b2}
    ] ++ lists:duplicate(200*1000, {b1, on, b3}) ++
    [ {b1, color, red}
    , {b2, on, table}
    , {b2, left_of, b3} ].

main () ->
    S = self(),
    lists:foreach(
      fun (Module) ->
              io:format("\t~p\n", [Module]),
              R = spawn(?MODULE, Module, [S]),
              timer:sleep(500),
              [R ! Fact || Fact <- facts()],
              receive
                  M -> io:format("-> ~p\n", [M])
              after 1000 ->
                      ok
              end,
              io:format("\n", [])
      end, [r]).

pinfo () ->
    Info0 = process_info(self()),
    Info  = lists:keydelete(messages, 1, Info0),
    io:format("<- ~p\n", [Info]).

doc (rule1) ->
    "(defrule find-stack-of-two-blocks-to-the-left-of-a-red-block"
        "    (<x> ^on <y>)"
        "    (<y> ^left-of <z>)"
        "    (<z> ^color red)"
        " => ( <z> ^on <x> ) )".

-define(A(X,Y), {X, on, Y}).       %% X Y
-define(B(Y,Z), {Y, left_of, Z}).  %%   Y Z
-define(C(Z)  , {Z, color, red}).  %%     Z
-define(R(X,Z), {Z, on, X}).

-define(ignore_anything_else
       , _M ->
               %%io:format("Skipped ~p\n", [_M]),
               r(Parent, Seen, Closure)).

r (Parent) -> r(Parent, [], '').
p (R, Pid) ->
    pinfo(),
    Pid ! R.

-record(abc, {x='', y='', z=''}).

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

%% Internals

%% compile (Alphas, Prod) ->
%%     try
%%         IDsAssocs = assoc(Alphas),
%%         Combis = perms(Alphas),
%%         {ok, Fun} = '2form'(Combis, IDsAssocs)
%%     catch
%%         M:E ->
%%             io:format("Error: ~p:~p ~p\n", [M,E,erlang:get_stacktrace()]),
%%             error
%%     end.

perms ([]) -> [[]];
perms (L) ->
    [[H|T] || H <- L,
              T <- perms(L -- [H])].

assoc (Alphas) ->
    IDs = lists:usort(
            lists:foldl(
              fun (Alpha, Acc) ->
                      Ids = extract_ids(Alpha),
                      Ids ++ Acc
              end, Alphas)),
    name(IDs, 0).

name ([ID|Rest], N) ->
    [{ID, "X"++integer_to_list(N)} | name(Rest, N+1)];
name ([], _) -> [].

extract_ids ({ID1, _Proposition, ID2}) ->
    [ID1, ID2].

%% End of Module.
