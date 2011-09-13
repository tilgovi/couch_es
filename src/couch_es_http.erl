%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of couch_es released under the Apache 2 license. 
%%% See the NOTICE for more information.


-module(couch_es_http).

-export([db_search_req/2, multidbs_search_req/1,
         db_percolator_req/2]).

-include_lib("couch/include/couch_db.hrl").

%% --------------------------------------------------------
%% http reqs
%% --------------------------------------------------------


%% @doc /db/_search handler
db_search_req(#httpd{path_parts=[DbName, <<"_search">>, Type]}=Req, _Db) ->
    Url = make_search_url(Req, DbName, Type),
    couch_es_proxy:handle_proxy_req(Req, Url);

db_search_req(#httpd{path_parts=[DbName, <<"_search">>]}=Req, _Db) ->
    Url = make_search_url(Req, DbName),
    couch_es_proxy:handle_proxy_req(Req, Url).

%% @doc /_search handler
multidbs_search_req(#httpd{path_parts=[<<"_search">>]}=Req) ->
    Path = string:join(["", "_search"], "/"),
    Url = couch_es_client:make_url(Path, couch_httpd:qs(Req)),
    couch_es_proxy:handle_proxy_req(Req, Url);

multidbs_search_req(#httpd{path_parts=[<<"_search">>, Dbs|_]}=Req) ->
    Path = string:join(["", binary_to_list(Dbs), "_search"], "/"),
    Url = couch_es_client:make_url(Path, couch_httpd:qs(Req)),
    couch_es_proxy:handle_proxy_req(Req, Url).

%% @doc /db/_percolator handler
db_percolator_req(#httpd{path_parts=[DbName, <<"_percolator">>]}=Req,
        _Db) ->
    Path = string:join(["", binary_to_list(DbName), "_percolator"]),
    Url = couch_es_client:make_url(Path, couch_httpd:qs(Req)),
    couch_es_proxy:handle_proxy_req(Req, Url).

%% --------------------------------------------------------
%% internal functions 
%% --------------------------------------------------------

make_search_url(Req, DbName) ->
    make_search_url(Req, DbName, DbName).

make_search_url(Req, DbName, Type) ->
    Path = string:join(["", binary_to_list(DbName), binary_to_list(Type), "_search"], "/"),
    couch_es_client:make_url(Path, couch_httpd:qs(Req)).
