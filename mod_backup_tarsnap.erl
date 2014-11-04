%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% Generated on 2014-10-31
%% @doc Tarsnap backup

-module(mod_backup_tarsnap).
-author("Arthur Clemens").

-mod_title("Tarsnap backup").
-mod_description("Manage backups of database and files to the Tarsnap online backup service.").
-mod_prio(600).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

-define(STATE_DEBUG, true).

-export([
    backup/1,
    list_backups/1,
    check_configuration/1,
    backup_in_progress/1
]).

% internal functions
-export([
    manage_schema/2,
    init/1,
    observe_admin_menu/3,
    debug/1,
    broadcast_error/2
]).


%% @doc Install the tables needed.
manage_schema(install, Context) ->
    %% This is a workaround for async initialization of the database tables; see github issues #734, #497
    backup_tarsnap_cache:init(z_context:prune_for_spawn(Context)),
    #datamodel{}.


init(Context) ->
    Cfg = check_configuration(Context),
    case Cfg of
        ok -> ok;
        Errors ->
            Msg = string:join(Errors, ", "),
            broadcast_error(Msg, Context),
            z_module_manager:deactivate(?MODULE, Context)
    end.


observe_admin_menu(admin_menu, Acc, _Context) ->
    [
     #menu_item{id=yaml_import,
                parent=admin_modules,
                label="Backup to Tarsnap",
                url={admin_backup_tarsnap},
                visiblecheck={acl, use, admin_backup_tarsnap}}
     |Acc].


backup(Context) ->
    backup_tarsnap_create:backup(Context).
 

list_backups(Context) ->
    backup_tarsnap_cache:get(Context).


check_configuration(Context) ->
    backup_tarsnap_service:check_configuration(Context).


backup_in_progress(_Context) ->
    lager:info(" backup_in_progress TODO"),
    false.


broadcast_error(Result, Context) ->
    z_session_manager:broadcast(#broadcast{type="error", message=Result, title="Tarsnap", stay=true}, z_acl:sudo(Context)).


debug(Msg) ->
    case ?STATE_DEBUG of
        true -> lager:info(Msg);
        _ -> undefined
    end.
