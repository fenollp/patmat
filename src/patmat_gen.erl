%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(patmat_gen).

%% patmat_gen: generate Erlang code for a rule.

-export([ file/1, file/2 ]).


-define(sep, "  \t").
-record(rule, {name, patterns, product}).

%% API

file (Ast) ->
    file(".", Ast).
file (Dir, Ast = #rule{name = Name}) ->
    Erlang = ast2erlang(Ast),
    Filename = filename:join(Dir, atom_to_list(Name)++".erl"),
    ok = file:write_file(Filename, Erlang),
    Filename.

%% Internals

ast2erlang (Ast = #rule{name=Name, patterns=Patterns, product=Product}) ->
    IDs  = ids(Patterns),
    Nmax = lists:seq(1, length(Patterns)),
    [ head(Name)
    , defrecord(IDs)
    , defpatterns(Patterns, 1, [])
    , defproduction(Product)
    , "\n%% API\n\n"
    , defast(Ast)
    , defhat()
    , defrule(Nmax, Patterns, Product)
    , foot() ].

%% Create a record “c” with lIDs as fields
defrecord (IDs) ->
    Fields = [atom_to_list(ID)++"=''" || ID <- IDs],
    ["-record(c, {", string:join(Fields, ", "), "}).\n\n"].

%% Create Erlang -defines to simplify patterns code
defpatterns ([Pattern|Rest], I, Acc) ->
    Def = [ "-define(P"
          , integer_to_list(I), "(", ids2args(Pattern), "), " ?sep
          , patt2erl(Pattern), ").\n" ],
    defpatterns(Rest, I+1, [Def|Acc]);
defpatterns ([], _, Acc) ->
    lists:reverse(Acc).

ids2args (Patt) ->
    string:join(lid(ids([Patt])), ",").

patt2erl ({{id,Lhs}, Info, {id,Rhs}}) ->
    ["{", lid(Lhs), ", ", pp(Info), ", ", lid(Rhs), "}"];
patt2erl ({{id,Lhs}, Info, Rhs}) ->
    ["{", lid(Lhs), ", ", pp(Info), ", ", pp(Rhs), "}"];
patt2erl ({Lhs, Info, {id,Rhs}}) ->
    ["{", pp(Lhs), ", ", pp(Info), ", ", lid(Rhs), "}"];
patt2erl ({Lhs, Info, Rhs}) ->
    ["{", pp(Lhs), ", ", pp(Info), ", ", pp(Rhs), "}"].

%% Erlang -define for the production
defproduction (Product) ->
    ["-define(R(", ids2args(Product), "), ", patt2erl(Product), ").\n"].

%% AST's IDs found in patterns
ids (Patterns) ->
    lists:usort(
      lists:flatmap(
        fun
            ({{id,Lhs}, _, {id,Rhs}}) -> [Lhs, Rhs];
            ({{id,Lhs}, _, _}) -> [Lhs];
            ({_, _, {id,Rhs}}) -> [Rhs];
            ({_, _, _}) -> []
        end, Patterns)).

%% ID as an Erlang variable (literal ID)
lid (IDs)
  when is_list(IDs) ->
    [lid(ID) || ID <- IDs];
lid (ID) ->
    [H|Rest] = atom_to_list(ID),
    [string:to_upper(H) | Rest].

%% Define the ast/0 function
defast (Ast) ->
    io_lib:format("ast () ->\n    ~p.\n\n", [Ast]).

%% Define rule's hat function
defhat () ->
    "r (Parent) ->\n    r(Parent, [], #c{}).\n\n".

perms ([]) -> [[]];
perms (L) ->
    [[H|T] || H <- L,
              T <- perms(L -- [H])].

powerset ([]) -> [[]];
powerset ([H|T]) ->
    PT = powerset(T),
    powerset(H, PT, PT).
powerset (_, [], Acc) -> Acc;
powerset (X, [H|T], Acc) ->
    powerset(X, T, [[X|H]|Acc]).

%% Define the actual rule ('s combinations)
defrule (Nmax, Patterns, Product) ->
    Combis = powerset(Nmax),
    [r_clause(Nmax, Patterns, Seen) || Seen <- Combis]
        ++ r_end(Nmax, Patterns, Product).

r_end (Nmax, Patterns, Product) ->
    Bindings = [[${,atom_to_list(ID),$,,lid(ID),$}] || ID <- ids(Patterns)],
    [ "r (Parent, Seen, #c{", ids2fields(Nmax,Patterns), "}) ->\n"
    , "    Parent ! ?R(", ids2args(Product), "),\n"
    , "    Info = process_info(self()),\n"
    %%, "    Info  = lists:keydelete(messages, 1, Info0),\n"
    , "    io:format(user, \"<- ~p\\n\", [Info]),\n"
    , "    Bindings = [", string:join(Bindings,", "), "],\n"
    , "    io:format(\"Seen = ~p | ~p\\n\", [Seen,Bindings]).%%\n" ].

r_clause (Nmax, Patterns, Seen) ->
    case Nmax -- Seen of
        [] -> ""; %% powerset/1 produces Nmax.
        UnSeen ->
            Rcvs = [ begin
                         [Pattern] = select_patterns([U], Patterns),
                         r_rcv(U, Pattern)
                     end || U <- UnSeen],
            r_combination(Patterns, Seen, Rcvs)
    end.

r_combination (Patterns, Seen, Rcvs) ->
    [ "r (Parent, Seen, Closure=#c{", ids2fields(Seen,Patterns), "})\n"
    , "  when ", seens(Seen), " ->\n"
    , "    receive\n"
    , [[string:chars($\s,8), Rcv, ";\n"] || Rcv <- Rcvs]
    , "        ?ignore_anything_else\n"
    , "    end;\n\n" ].

r_rcv (N, Pattern) ->
    Nstr = integer_to_list(N),
    [ "?P", Nstr
    , "(", ids2args(Pattern), ") " ?sep "-> r(Parent, ["
    , Nstr, "|Seen], Closure#c{", ids2fields(Pattern), "})" ].

seens (Seen) ->
    SameSeens = perms(Seen),
    Perms  = ["Seen == "++pp(S) || S <- SameSeens],
    string:join(Perms, " orelse ").

ids2fields (IDs)
  when is_list(IDs) ->
    Assocs = [atom_to_list(ID) ++"="++ lid(ID) || ID <- IDs],
    string:join(Assocs, ", ");
ids2fields (Pattern) ->
    ids2fields(ids([Pattern])).

ids2fields (Ns, Patterns) ->
    Pats = select_patterns(Ns, Patterns),
    ids2fields(ids(Pats)).

select_patterns (Ns, Patterns) ->
    select_patterns(Ns, Patterns, 1, []).
select_patterns (_, [], _, Acc) -> Acc;
select_patterns (Ns, [Pat|Rest], I, Acc) ->
    case lists:member(I, Ns) of
        true  -> NewAcc = [Pat|Acc];
        false -> NewAcc = Acc
    end,
    select_patterns(Ns, Rest, I + 1, NewAcc).

pp (Data) ->
    io_lib:format("~p", [Data]).

head (Name) ->
    io_lib:format("%% Code auto-generated by module ~p
%% -*- coding: utf-8 -*-
-module(rule1).
-compile(nowarn_unused_vars).

%% rule: ~p.

-export([ r/1
        , ast/0 ]).


-define(ignore_anything_else
       , eof -> ok;
         _M  ->
             r(Parent, Seen, Closure)).
\n", [?MODULE, Name]).

foot () ->
    "\n%% End of Module.\n".

%% End of Module.
