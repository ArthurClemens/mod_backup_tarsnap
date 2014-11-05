%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% @doc Tarsnap backup

-module(mod_backup_tarsnap).
-author("Arthur Clemens").
-behaviour(gen_server).

-mod_title("Tarsnap backup").
-mod_description("Manage backups of database and files to the Tarsnap online backup service.").
-mod_prio(600).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

-export([
    refresh/1,
    list_archives/1,
    check_configuration/1,
    backup_in_progress/1
]).

% internal functions
-export([
    manage_schema/2,
    observe_admin_menu/3,
    debug/2,
    broadcast_error/2
]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-record(state, {context, backup_start, backup_pid, timer_ref}).

% Interval for checking for new and/or changed files.
-define(BCK_POLL_INTERVAL, 1000 * 60 * 10). % in milliseconds, so 10 minutes


%% @doc Install the tables needed.
manage_schema(install, Context) ->
    %% This is a workaround for async initialization of the database tables; see github issues #734, #497
    backup_tarsnap_cache:init(z_context:prune_for_spawn(Context)),
    #datamodel{}.


observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=yaml_import,
                parent=admin_modules,
                label=?__("Backup (Tarsnap)", Context),
                url={admin_backup_tarsnap},
                visiblecheck={acl, use, admin_backup_tarsnap}}
     |Acc].

 
refresh(Context) ->
    Archives = backup_tarsnap_service:archives(Context),
    Cfg = backup_tarsnap_service:check_configuration(Context),
    case proplists:get_value(ok, Cfg) of
        true ->
            ArchiveData = backup_tarsnap_service:archive_data(Archives, Context),
            {ok, ArchiveData};
        false ->
            {error, "Tarsnap is not configured properly."}
    end.


list_archives(Context) ->
    backup_tarsnap_cache:get(Context).


check_configuration(Context) ->
    case backup_in_progress(Context) of
        true -> 
            % assume that previous checks were ok
            % otherwise no backup could be in progress
            [
                {busy, true}
            ];
        false -> 
            backup_tarsnap_service:check_configuration(Context)
    end.


backup_in_progress(Context) ->
    case gen_server:call(z_utils:name_for_host(?MODULE, z_context:site(Context)), in_progress_start) of
        undefined -> false;
        _ -> true
    end.


broadcast_error(Result, Context) ->
    z_session_manager:broadcast(#broadcast{type="error", message=Result, title="Tarsnap", stay=true}, z_acl:sudo(Context)).


debug(Msg, Context) ->
    case z_convert:to_bool(m_config:get_value(
        mod_backup_tarsnap,
        debug,
        Context
    )) of
        true -> lager:info(Msg);
        _ -> undefined
    end.


%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    Context = proplists:get_value(context, Args),
    Name = z_utils:name_for_host(?MODULE, z_context:site(Context)),
    gen_server:start_link({local, Name}, ?MODULE, Args, []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    case z_db:table_exists(mod_backup_tarsnap_cache, Context) of
        false ->
            backup_tarsnap_cache:init(Context);
        true ->
            ok
    end,
    {ok, TimerRef} = timer:send_interval(?BCK_POLL_INTERVAL, periodic_backup),
    State = #state{
        context = z_context:new(Context),
        backup_pid = undefined,
        timer_ref = TimerRef
    },
    % create first backup
    Pid = do_backup(State),
    {ok, State#state{
        backup_pid = Pid,
        backup_start=calendar:universal_time()
    }}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Start a backup
handle_call(start_backup, _From, State) ->
    case State#state.backup_pid of
        undefined ->
            Pid = do_backup(State),
            {reply, ok, State#state{
                backup_pid=Pid,
                backup_start=calendar:universal_time()
            }};
        _Pid ->
            {reply, {error, in_progress}, State}
    end;

%% @doc Returns boolean
handle_call(in_progress_start, _From, State) ->
    {reply, State#state.backup_start, State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Periodic check if a scheduled backup should start
handle_info(periodic_backup, #state{backup_pid=Pid} = State) when is_pid(Pid) ->
    z_utils:flush_message(periodic_backup),
    {noreply, State};
handle_info(periodic_backup, State) ->
    z_utils:flush_message(periodic_backup),
    Pid = do_backup(State),
    {noreply, State#state{
        backup_pid=Pid,
        backup_start=calendar:universal_time()
    }};

handle_info({'EXIT', Pid, normal}, State) ->
    case State#state.backup_pid of
        Pid ->
            %% @todo send an update to the page that started the backup
            {noreply, State#state{
                backup_pid=undefined,
                backup_start=undefined
            }};
        _ ->
            {noreply, State}
    end;

handle_info({'EXIT', _Pid, _Error}, State) ->
    {noreply, State};

%% @doc Handling all non call/cast messages
handle_info(Info, State) ->
    ?DEBUG(Info),
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    timer:cancel(State#state.timer_ref),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @doc Start a backup and return the pid of the backup process, whilst linking to the process.
do_backup(State) ->
    spawn_link(fun() -> do_backup_process(State#state.context) end).
    
do_backup_process(Context) ->
    backup_tarsnap_create:backup(Context),
    backup_tarsnap_delete:delete(Context).
