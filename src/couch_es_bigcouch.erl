%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of couch_es released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(couch_es_bigcouch).

-export([can_index/1, get_all_dbs/0, get_prefix/0]).

can_index(DbName0) ->
    Db1 = list_to_binary(couch_config:get("mem3", "node_db", "nodes")),
    Db2 = list_to_binary(couch_config:get("mem3", "shard_db", "dbs")),
    Db3 = list_to_binary(couch_config:get("couch_httpd_auth", "authentication_db",
        "_users")),

    IgnoredDbs = [Db1, Db2, Db3] ++ re:split(couch_config:get("couch_es",
            "ignore_dbs",""), "\\s*,\\s*",[{return, list}]),

    DbName = mem3:dbname(DbName0),

    case [DbName] -- IgnoredDbs of
    [] -> no_index;
    _ ->
        %% is replicator db ?
        {ok, RegExp} = re:compile("^([a-z][a-z0-9\\_\\$()\\+\\-\\/]*/)?_replicator$"),
        Match = re:run(DbName, RegExp, [{capture,none}]),
        if match =/=  Match ->
                {ok, DbName};
            true -> no_index
        end
    end.

get_all_dbs() ->
    %% get all local dbs.
    {ok, DbNames} = couch_server:all_databases(),
    lists:usort([mem3:dbname(DbName) || DbName <- DbNames]).

%% the logig with bigcouch is to use use one elasticsearch cluster for
%% one bigcouch cluster. So no need for a prefix.
get_prefix() ->
    "". 
