%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(patmat).

%% patmat: 

-export([ p/2 ]).

-export_type([pattern/0, value/0, id/0]).

%% API

-type pattern() :: {value(), atom(), value()}.
-type value() :: id() | number() | atom().
-type id() :: {id, atom()}.

p (R, Pid) ->
    Info0 = process_info(self()),
    Info  = lists:keydelete(messages, 1, Info0),
    io:format(user, "<- ~p\n", [Info]),
    Pid ! R.

%% Internals

%% End of Module.
