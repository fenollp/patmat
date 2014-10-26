%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(patmat).

%% patmat: 

-export([  ]).

-export_type([pattern/0, value/0, id/0]).

%% API

-type pattern() :: {value(), atom(), value()}.
-type value() :: id() | number() | atom().
-type id() :: {id, atom()}.

%% Internals

%% End of Module.
