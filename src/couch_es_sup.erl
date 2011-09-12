%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of bigcouch_es released under the Apache 2 license. 
%%% See the NOTICE for more information.


-module(couch_es_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    %% open es_db_listener worker and wait for the child.
    Children = [
        {couch_es_sync,
            {couch_es_sync, start_link, []},
            permanent, infinity, supervisor, [couch_es_sync]},
        {es_watcher,
            {couch_es_watcher, start_link, []},
            permanent, 5000, worker, [couch_es_watcher]}
    ],
    {ok, {{one_for_all, 10, 10}, Children}}.


