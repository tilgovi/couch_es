%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of couch_es released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(couch_es_couchdb).

-export([can_index/1, get_all_dbs/0, get_prefix/0]).

can_index(DbName) ->
    Db1 = list_to_binary(couch_config:get("couch_httpd_auth", 
            "authentication_db", "_users")),
    Db2 = list_to_binary(couch_config:get("replicator", "db",
            "_replicator")),
   
    IgnoredDbs = [Db1, Db2] ++ re:split(couch_config:get("couch_es",
            "ignore_dbs",""), "\\s*,\\s*",[{return, list}]),
        
    case [DbName] -- IgnoredDbs of
    [] -> no_index;
    _ -> {ok, DbName}
    end.

get_all_dbs() ->
    {ok, DbNames} = couch_server:all_databases(),
    DbNames.

get_prefix() ->
    %% useful for multinode installation
    case couch_config:get("couch_es", "prefix", "") of
    "_node" -> node();
    Prefix -> Prefix
    end.
