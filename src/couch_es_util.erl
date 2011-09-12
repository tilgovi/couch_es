%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of couch_es released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(couch_es_util).

-export([get_backend_module/1, prefixed_name/2]).

get_backend_module(BackendStr) ->
    case list_to_atom(BackendStr) of
        couchdb -> couch_es_couchdb;
        bigcouch -> couch_es_bigcouch;
        Backend -> Backend
    end.

prefixed_name(Prefix, Name) ->
    case Prefix of
    "" -> Name;
    _ -> Prefix ++ "_" ++ couch_util:to_list(Name)
    end.
