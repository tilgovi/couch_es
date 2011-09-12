%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of couch_es released under the Apache 2 license. 
%%% See the NOTICE for more information.

%% @doc check db events and manage rivers creation/deletion

-module(couch_es_watcher).

-behaviour(gen_server).

-export([start_link/0, config_changes/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).


-include_lib("couch/include/couch_db.hrl").

-record(state, {
        db_notifier = nil,
        scan_pid = nil,
        enabled,
        mod}).

-define(SERVER, ?MODULE).

%% --------------------------------------------------------
%% public API 
%% --------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

config_changes("couch_es", "backend", V) ->
    gen_server:cast(?MODULE, {set_backend, V});
config_changes("couch_es", "enabled", V) ->
    gen_server:cast(?MODULE, {enable, couch_es_util:trim_whitespace(V)});
config_changes(_, _, _) ->
    ok.


%% --------------------------------------------------------
%%  gen_server callbacks
%% --------------------------------------------------------

init(_) ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),

    BackendStr = couch_config:get("couch_es", "backend",
        "couchdb"),
    Mod = couch_es_util:get_backend_module(BackendStr),

    ok = couch_config:register(fun ?MODULE:config_changes/3),

    IsEnabled = couch_config:get("couch_es", "enabled", "yes"),

    InitState = case couch_es_util:trim_whitespace(IsEnabled) of
        "yes" ->
            %% start dbs notifications process
            NotifierPid = db_update_notifier(Mod),

            %% initalize index
            ScanPid = spawn_link(fun() -> scan_all_dbs(Mod) end),

            #state{db_notifier = NotifierPid,
                        scan_pid = ScanPid,
                        mod = Mod,
                        enabled = true};
        "no" ->
            #state{mod = Mod, enabled = false}
    end,
    {ok, InitState}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({enable, "yes"}, #state{enabled=Enabled, mod=Mod}=State) ->
    NewState = case Enabled of
        true -> State;
        false ->
            NotifierPid = db_update_notifier(Mod),
            ScanPid = spawn_link(fun() -> scan_all_dbs(Mod) end),
            #state{db_notifier = NotifierPid,
                        scan_pid = ScanPid,
                        enabled = true}
    end,

    {noreply, NewState};

handle_cast({enable, "no"}, #state{enabled=Enabled, db_notifier=NPid,
            scan_pid=SPid}=State) ->
    NewState = case Enabled of
        false -> State;
        true ->
            couch_util:shutdown_sync(NPid),
            couch_util:shutdown_sync(SPid),
            #state{db_notifier = nil, scan_pid = nil, enabled = false}
    end,


    {noreply, NewState};

handle_cast({set_backend, BackendStr}, State) ->
    Mod = couch_es_util:get_backend_module(BackendStr),
    {noreply, State#state{mod = Mod}};
    
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodeup, _Node}, State) ->
    {noreply, refresh_all_indexes(State)};

handle_info({nodedown, _Node}, State) ->
    {noreply, refresh_all_indexes(State)};

handle_info({'EXIT', From, normal}, #state{scan_pid = From} = State) ->
    {noreply, State#state{scan_pid=nil}};

handle_info({'EXIT', From, Reason}, #state{scan_pid = From} = State) ->
    {stop, {scanner_died, Reason}, State};

handle_info({'EXIT', From, Reason}, #state{db_notifier = From} = State) ->
    {stop, {db_update_notifier_died, Reason}, State};

handle_info(Msg, State) ->
    ?LOG_INFO("~p got an unexpected message: ~p", [?MODULE, Msg]),
    {noreply, State}.

terminate(_Reason, State) ->
    #state{
        scan_pid = ScanPid,
        db_notifier = DbNotifier
    } = State,
  
    couch_util:shutdown_sync(ScanPid), 
    couch_db_update_notifier:stop(DbNotifier),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------
%% internal functions 
%% --------------------------------------------------------

refresh_all_indexes(#state{scan_pid=nil, mod=Mod}=State) ->
    ?LOG_DEBUG("recreate indexes if needed", []),
    Pid = spawn_link(fun() -> scan_all_dbs(Mod) end),
    State#state{scan_pid=Pid};
refresh_all_indexes(#state{scan_pid=Pid}=State) ->
    couch_util:shutdown_sync(Pid),
    refresh_all_indexes(State#state{scan_pid=nil}).

db_update_notifier(Mod) ->
    {ok, Notifier} = couch_db_update_notifier:start_link(fun
        ({deleted, DbName}) ->
            handle_db_event(Mod, {delete, DbName});
        ({created, DbName}) ->
            handle_db_event(Mod, {create, DbName});
        ({updated, DbName}) ->
            ok 
    end),
    Notifier.

handle_db_event(Mod, {Evt, DbName0}) ->
    case Mod:can_index(DbName0) of
    {ok, DbName} ->
        couch_es_sync:push({Evt, DbName});
    _ ->
        ok
    end.
    
scan_all_dbs(Mod) ->
    AllDbs = Mod:get_all_dbs(),
    scan_all_dbs(Mod, AllDbs).

scan_all_dbs(_Mod, []) ->
    ok;
scan_all_dbs(Mod, [Db|Rest]) ->
    handle_db_event(Mod, {create, Db}),
    scan_all_dbs(Mod, Rest).
