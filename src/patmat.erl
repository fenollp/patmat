%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(patmat).

%% patmat: 

-export([ facts/0
        , doc/1
        , rule/2
        , rule1/1
        ]).


%% API

facts () ->
    [ {b1, on, b2}
    , {b1, on, b3}
    , {b1, color, red}
    , {b2, on, table}
    , {b2, left_of, b3} ].

doc (rule1) ->
    "(defrule find-stack-of-two-blocks-to-the-left-of-a-red-block"
        "    (<x> ^on <y>)"
        "    (<y> ^left-of <z>)"
        "    (<z> ^color red)"
        " => ( <z> ^on <x> ) )".

%% %% No closure of X,Y
%% rule (1, Parent, 1) ->
%%     receive
%%         {X, on, Y} ->
%%             rule(1, Parent, 2);
%%         _ ->
%%             ignore
%%     end;

production (Rule, Pid) ->
    Pid ! Rule.

-define(drop_others
       , _ ->
               ignore).

-define(alpha(Pattern, Then)
       , receive
             Pattern ->
                 Then;
             ?drop_others
         end).

rule (1, Parent) ->
    ?alpha( {X, on, Y}
          , ?alpha( {Y, left_of, Z}
                  , ?alpha( {Z, color, red}
                          , production({Z, on, X}, Parent)))).
 
rule1 (Parent) ->
    receive
        {X, on, Y} ->
            receive
                {Y, left_of, Z} ->
                    receive
                        {Z, color, red} ->
                            Parent ! {Z, on, X};
                        _ ->
                            ignore
                    end;
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end.

-define(A, {X, on, Y}).
-define(B, {Y, left_of, Z}).
-define(C, {Z, color, red}).
-define(R, {Z, on, X}).

rule1_ (Parent) ->
    receive
        ?A ->
            receive
                ?B ->
                    ?alpha( ?C , production(?R,Parent)); %% a b c
                ?C ->
                    ?alpha( ?B , production(?R,Parent)); %% a c b
                ?drop_others
            end;
        ?B ->
            receive
                ?A ->
                    ?alpha( ?C , production(?R,Parent)); %% b a c
                ?C ->
                    ?alpha( ?A , production(?R,Parent)); %% b c a
                ?drop_others
            end;
        ?C ->
            receive
                ?A ->
                    ?alpha( ?B , production(?R,Parent)); %% c a b
                ?B ->
                    ?alpha( ?A , production(?R,Parent)); %% c b a
                ?drop_others
            end;
        ?drop_others
    end.

%% Internals

compile (Alphas, Prod) ->
    try
        IDsAssocs = extract_ids(Alphas),
        Combis = perms(Alphas),
        {ok, Fun} = '2form'(Combis, IDsAssocs)
    catch
        M:E ->
            io:format("Error: ~p:~p ~p\n", [M,E,erlang:get_stacktrace()]),
            error
    end.

perms ([]) -> [[]];
perms (L) ->
    [[H|T] || H <- L,
              T <- perms(L -- [H])].

extract_ids (Alphas) ->
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
