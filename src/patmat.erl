%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(patmat).

%% patmat: 

-export([ facts/0
        , doc/1
        , rule/2, rule1/1, rule1_/2
        ]).


%% API

facts () ->
    [ {b3, color, red}
    , {b1, on, b2}
    , {b1, on, b3}
    , {b1, color, red}
    , {b2, on, table}
    , {b2, left_of, b3} ].

main () ->
    S = self(),
    lists:foreach(
      fun (Module) ->
              io:format("\t~p\n", [Module]),
              R = spawn(?MODULE, Module, [1,S]),
              timer:sleep(500),
              [R ! Fact || Fact <- facts()],
              receive
                  M -> io:format("-> ~p\n", [M])
              after 1000 ->
                      ok
              end,
              io:format("\n", [])
      end, [rule, rule1_, r_]).

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
               io:format("Skipped ~p\n", [_M]),
               r_(Parent, Seen, Closure)).

r (N, Parent) -> r(N, Parent, 1).
r_ (_N, Parent) -> r_(Parent, [], '').
p (N, R, Pid) ->
    Pid ! R,
    io:format("Rule ~p produced ~p\n", [N,R]).

-record(abc, {x='', y='', z=''}).

r_ (Parent, Seen=[], Closure) ->
    receive
        ?A(X,Y) -> r_(Parent, [a], #abc{x=X, y=Y});
        ?B(Y,Z) -> r_(Parent, [b], #abc{y=Y, z=Z});
        ?C(Z)   -> r_(Parent, [c], #abc{z=Z});
        ?ignore_anything_else
    end;

r_ (Parent, Seen=[a], Closure=#abc{x=X, y=Y}) ->
    receive
        ?B(Y,Z) -> r_(Parent, Seen++[b], Closure#abc{z=Z});
        ?C(Z)   -> r_(Parent, Seen++[c], Closure#abc{z=Z});
        ?ignore_anything_else
    end;

r_ (Parent, Seen=[b], Closure=#abc{y=Y, z=Z}) ->
    receive
        ?A(X,Y) -> r_(Parent, Seen++[a], Closure#abc{x=X});
        ?C(Z)   -> r_(Parent, Seen++[c], Closure);
        ?ignore_anything_else
    end;

r_ (Parent, Seen=[c], Closure=#abc{z=Z}) ->
    receive
        ?A(X,Y) -> r_(Parent, Seen++[a], Closure#abc{x=X, y=Y});
        ?B(Y,Z) -> r_(Parent, Seen++[b], Closure#abc{y=Y});
        ?ignore_anything_else
    end;

r_ (Parent, Seen, Closure=#abc{x=X, y=Y, z=Z})
  when Seen == [a,b] orelse Seen == [b,a] ->
    receive
        ?C(Z)   -> r_(Parent, Seen++[c], Closure);
        ?ignore_anything_else
    end;

r_ (Parent, Seen, Closure=#abc{x=X, y=Y, z=Z})
  when Seen == [a,c] orelse Seen == [c,a] ->
    receive
        ?B(Y,Z) -> r_(Parent, Seen++[b], Closure);
        ?ignore_anything_else
    end;

r_ (Parent, Seen, Closure=#abc{y=Y, z=Z})
  when Seen == [b,c] orelse Seen == [c,b] ->
    receive
        ?A(X,Y) -> r_(Parent, Seen++[a], Closure#abc{x=X});
        ?ignore_anything_else
    end;

r_ (Parent, Seen, #abc{x=X, y=Y, z=Z})
  when length(Seen) == 3 ->
    io:format("Seen = ~p\n", [Seen]),%%
    p(1, ?R(X,Z), Parent).

-define(A, {X, on, Y}).       %% X Y
-define(B, {Y, left_of, Z}).  %%   Y Z
-define(C, {Z, color, red}).  %%     Z
-define(R, {Z, on, X}).

r (N=1, Parent, 1) ->
    io:format("In N=1 1\n",[]),%%
    receive
        ?A ->
            r(N, Parent, 2, X, Y);
        _M ->
            io:format("Skip ~p\n",[_M]),%%
            r(N, Parent, 1)
    end.

r (N=1, Parent, 2, X, Y) ->
    io:format("In N=1 2 ~p ~p\n",[X,Y]),%%
    receive
        ?B ->
            r(N, Parent, 3, X, Y, Z);
        _M ->
            io:format("Skip ~p\n",[_M]),%%
            r(N, Parent, 2, X, Y)
    end.

r (N=1, Parent, 3, X, Y, Z) ->
    io:format("In N=1 2 ~p ~p ~p\n",[X,Y,Z]),%%
    receive
        ?C ->
            p(N, ?R, Parent);
        _M ->
            io:format("Skip ~p\n",[_M]),%%
            r(N, Parent, 3, X, Y, Z)
    end.


production (Rule, Pid) ->
    Pid ! Rule.

-define(drop_others
       , _ ->
               ignore).

-define(alpha(Pattern, Then)
       , receive
             Pattern ->
                 Then%;
             %?drop_others
         end).

rule (1, Parent) ->
    ?alpha( ?A
          , ?alpha( ?B
                  , ?alpha( ?C
                          , production(?R, Parent)))).

rule1 (Parent) ->
    receive
        ?A ->
            receive
                ?B ->
                    receive
                        ?C ->
                            Parent ! ?R%;
                        %_ -> ignore
                    end%;
                %_ -> ignore
            end%;
        %_ -> ignore
    end.


rule1_ (_, Parent) ->
    receive
        ?A ->
            receive
                ?B ->
                    ?alpha( ?C , production(?R,Parent)); %% a b c
                ?C ->
                    ?alpha( ?B , production(?R,Parent))%; %% a c b
                %?drop_others
            end;
        ?B ->
            receive
                ?A ->
                    ?alpha( ?C , production(?R,Parent)); %% b a c
                ?C ->
                    ?alpha( ?A , production(?R,Parent))%; %% b c a
                %?drop_others
            end;
        ?C ->
            receive
                ?A ->
                    ?alpha( ?B , production(?R,Parent)); %% c a b
                ?B ->
                    ?alpha( ?A , production(?R,Parent))%; %% c b a
                %?drop_others
            end%;
        %?drop_others
    end.

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
