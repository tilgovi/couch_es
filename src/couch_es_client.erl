%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of bigcouch_es released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(couch_es_client).

-export([create_index/2, delete_index/1,
         save_mapping/3, delete_mapping/2,
         make_url/1, make_url/2]).

-include_lib("couch/include/couch_db.hrl").

-define(TIMEOUT, infinity).


create_index(DbName, Name) ->
    Doc = make_river_doc(DbName, Name, Name),
    Url = make_url(["/_river/", couch_util:to_list(Name), "/_meta"]),
    case request(put, Url, ["200", "201"], [], [], ?JSON_ENCODE(Doc)) of
    {ok, _Status, _Headers, _Body} ->
        ok;
    Error ->
        ?LOG_ERROR("Can't send river ~p on ~p : ~p",
            [?JSON_ENCODE(Doc), Url, Error]),
        Error 
    end.

delete_index(Name) ->
    ok = delete_river(Name),
    ok = delete_db_index(Name).

delete_river(Name) ->
    Url = make_url(["/_river/", couch_util:to_list(Name)]),
    case request(delete, Url, ["200", "201", "404"], []) of
    {ok, _, _, _Body} ->
        ok;
    Error ->
        ?LOG_ERROR("can't delete index for ~p ~p", [Name,
                Error])
    end.

%% @doc delete index
delete_db_index(Name) ->
    Url = make_url(["/", couch_util:to_list(Name)]),
    case request(delete, Url, ["200", "201", "404"], []) of
    {ok, _, _, _Body} ->
        ok;
    Error ->
        ?LOG_ERROR("can't delete index for ~p ~p", [Name,
                Error])
    end.


save_mapping(DbName, Name, Mapping) ->
    Url = make_url(["/", couch_util:to_list(DbName), "/",
            couch_util:to_list(Name), "/_mapping"]),

    case request(put, Url, ["200"], [], [], ?JSON_ENCODE(Mapping)) of
    {ok, _Status, _Headers, _Body} ->
        ok;
    Error ->
        ?LOG_ERROR("Can't send mapping ~p on ~p : ~p",
            [?JSON_ENCODE(Mapping), Url, Error]),
        Error 
    end.

delete_mapping(DbName, Name) ->
    Url = make_url(["/", couch_util:to_list(DbName), "/",
            couch_util:to_list(Name)]),
    case request(delete, Url, ["200", "404"], []) of
    {ok, _Status, _Headers, _Body} ->
        ok;
    Error ->
        ?LOG_ERROR("Can't delete mapping ~p", [Error]),
        Error 
    end.

%% @doc send an ibrowse request
request(Method, Url, Expect, Options) ->
    request(Method, Url, Expect, Options, [], []).
request(Method, Url, Expect, Options, Headers) ->
    request(Method, Url, Expect, Options, Headers, []).
request(Method, Url, Expect, Options, Headers, Body) ->
    Accept = {"Accept", "application/json, */*;q=0.9"},
    case ibrowse:send_req(Url, [Accept|Headers], Method, Body, 
            [{response_format, binary}|Options], ?TIMEOUT) of
        Resp={ok, Status, _, _} ->
            case lists:member(Status, Expect) of
                true -> Resp;
                false -> {error, Resp}
            end;
        Error -> Error
    end.

%% @doc return elasticsearch url
es_address() ->
    couch_config:get("bigcouch_es", "address", "localhost:9200").

make_url(Path) ->
    make_url(Path, []).

make_url(Path, Query) ->
    binary_to_list(
        iolist_to_binary(
            ["http://",
             es_address(),
             Path,
             [ ["?", mochiweb_util:urlencode(Query)] || Query =/= [] ]
            ])).


make_river_doc(DbName, Index, Type) ->
    Host = ?l2b(couch_config:get("couch_es", "public_host", "localhost")),
    Port = list_to_integer(couch_config:get("couch_es", "public_port",
            "5984")),
    BulkSize = list_to_integer(couch_config:get("couch_es", "bulk_size",
            "100")),
    BulkTimeOut =  list_to_integer(couch_config:get("couch_es",
            "bulk_timeout", "10")),

    
    CouchDBProps0 = [{<<"host">>, Host},
                    {<<"port">>, Port},
                    {<<"db">>, DbName},
                    {<<"filter">>, null}],

    %% TODO: customize couchdb river plugin to add a better auth system
    CouchDBProps = case couch_config:get("couch_es", "username") of
        undefined ->
            CouchDBProps0;
        UserName ->
            case couch_config:get("couch_es", "password") of
            undefined ->
                [{<<"username">>, ?l2b(UserName)}|CouchDBProps0];
            Password ->
                [{<<"username">>, ?l2b(UserName)}, {<<"password">>,
                        ?l2b(Password)} | CouchDBProps0]
            end
        end,

    Props= [{<<"type">>, <<"couchdb">>}] 
        ++ [{<<"couchdb">>, {CouchDBProps}}]
        ++ [{<<"index">>, {[
                {<<"index">>, Index},
                {<<"type">>, Type},
                {<<"bulk_size">>, BulkSize},
                {<<"bulk_timeout">>, BulkTimeOut}]}}
            ],
    {Props}.
