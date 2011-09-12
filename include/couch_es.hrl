%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of bigcouch_es released under the Apache 2 license. 
%%% See the NOTICE for more information.


-define(ADMIN_CTX, {user_ctx, #user_ctx{roles=[<<"_admin">>]}}).

-record(mapping, {
        db_name,
        name,
        script=nil}).
