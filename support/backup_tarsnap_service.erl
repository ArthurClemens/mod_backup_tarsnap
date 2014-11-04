%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% Generated on 2014-10-31
%% @doc Tarsnap command/service related functions

-module(backup_tarsnap_service).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

-export([
    check_configuration/1,
    archives/1,
    store/2,
    remove/1
]).
  
check_configuration(Context) ->
    Errors = lists:foldl(fun(Result, Acc) ->
        case Result of
            ok -> Acc;
            _ ->
                {error, Message} = Result,
                [[Message]|Acc]
        end
    end, [], [
        check_installed(archive_cmd()),
        check_installed(db_dump_cmd()),
        check_installed(tarsnap_cmd()),
        is_tarsnap_configured(Context)
    ]),
    case Errors of
        [] -> ok;
        _ -> lists:concat(Errors)
    end.
    

check_installed({Name, Cmd}) ->
    case which(Cmd) of
        [] -> {error, Name ++ " cannot be found"};
        _ -> ok
    end.

archive_cmd() ->
    {"tar", z_convert:to_list(z_config:get(tar, "tar"))}.

db_dump_cmd() ->
    {"pg_dump", z_convert:to_list(z_config:get(pg_dump, "pg_dump"))}.
    
tarsnap_cmd() ->
    {"tarsnap", z_convert:to_list(z_config:get(tarsnap, "tarsnap"))}.

which(Cmd) ->
    filelib:is_regular(z_string:trim_right(os:cmd("which " ++ z_utils:os_escape(Cmd)))).
     

%% If tarsnap is not configured properly, tarsnap will return a message complaining about a missing keyfile.
is_tarsnap_configured(Context) ->
    ProcessingDir = z_path:files_subdir_ensure("processing", Context),
    Cmd = "tarsnap -c -f test --dry-run " ++ ProcessingDir,
    Result = os:cmd(Cmd),
    case re:run(Result, "All archives") of 
        {match, _Match} -> 
            ok;
        _ -> 
            {error, Result}
    end.
    
%% Returns a list of archives
archives(Context) ->
    case check_configuration(Context) of
        ok -> 
            Cmd = "tarsnap --list-archives",
            Result = os:cmd(Cmd),
            string:tokens(Result, "\n");
        _ -> 
            ?zWarning("Tarsnap not installed or configured", Context),
            []
    end.

 
store(Name, Path) ->
    % options:
    % -c: Create an archive containing the specified items and name.
    % -n: Do not recursively archive the contents of directories.
    % -f: Archive name
    TarsnapCmd = "tarsnap -c -n -f " ++ Name ++ " " ++ Path,
    os:cmd(TarsnapCmd).


remove(Name) ->
    % options:
    % -d: Delete the specified archive
    % -f: Archive name
    TarsnapCmd = "tarsnap -d -f " ++ Name,
    os:cmd(TarsnapCmd).
    