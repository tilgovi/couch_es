%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of couch_es released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(couch_es_sync).

-export([start_link/0, config_changes/3,
         push/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-include_lib("couch/include/couch_db.hrl").

-define(SERVER, ?MODULE).

-record(state, {
    limit,
    mod,
    count = 0,
    refs,
    waiting = queue:new()}).

%% --------------------------------------------------------
%% public API 
%% --------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

push(Cmd) ->
    gen_server:cast(?MODULE, {push, Cmd}).

config_changes("couch_es", "backend", V) ->
    gen_server:cast(?MODULE, {set_backend, V});
config_changes("couch_es", "concurrency", V) ->
    gen_server:cast(?MODULE, list_to_integer(V)).

%% --------------------------------------------------------
%%  gen_server callbacks
%% ------------------

init(_) ->
    Limit = list_to_integer(
        couch_config:get("couch_es", "concurrency", "10")
    ),
    BackendStr = couch_config:get("couch_es", "backend",
        "couchdb"),

    Mod = couch_es_util:get_backend_module(BackendStr),

    ok = couch_config:register(fun ?MODULE:config_changes/3),
    {ok, #state{limit=Limit, mod=Mod, refs=gb_sets:empty()}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.


handle_cast({push, Cmd}, #state{limit=L, count=C, waiting=W}=State) 
        when C >= L ->    
    {noreply, State#state{waiting=queue:in(Cmd, W)}};

handle_cast({push, Cmd}, #state{mod=Mod, count=C, refs=R}=S) ->
    Prefix = Mod:get_prefix(),
    Pid = spawn_link(fun() -> do_sync(Cmd, Prefix) end),
    Ref = erlang:monitor(process, Pid),
    {noreply, S#state{count=C+1, refs=gb_sets:add(Ref,R)}};

handle_cast({set_concurrency, L}, State) ->
    {noreply, State#state{limit=L}};

handle_cast({set_backend, BackendStr}, State) ->
    Mod = couch_es_util:get_backend_module(BackendStr),
    {noreply, State#state{mod = Mod}};


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{refs=Refs}) ->
    case gb_sets:is_element(Ref, Refs) of
    true ->
        handle_down_sync(Ref, S);
    false ->
        {noreply, S}
    end;

handle_info(Msg, State) ->
    ?LOG_INFO("~p got an unexpected message: ~p", [?MODULE, Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%% --------------------------------------------------------
%% internal functions 
%% --------------------------------------------------------

do_sync({Action, DbName}, Prefix) ->
    Name = couch_es_util:prefixed_name(Prefix, DbName),
    case Action of
    delete ->
        couch_es_client:delete_index(Name);
    create ->
        couch_es_client:create_index(DbName, Name)
    end.

handle_down_sync(Ref, S = #state{count=C, refs=Refs, mod=Mod}) ->
    case queue:out(S#state.waiting) of
    {{value, Cmd}, Q} ->
        Prefix = Mod:get_prefix(),
        Pid = spawn_link(fun() -> do_sync(Cmd, Prefix) end),
        NewRef = erlang:monitor(process, Pid),
        NewRefs = gb_set:insert(NewRef, gb_sets:delete(Ref,Refs)),
        {noreply, S#state{waiting=Q, refs=NewRefs}};
    {empty, _} ->
        {noreply, S#state{count=C-1, refs=gb_sets:delete(Ref,Refs)}}
    end.
