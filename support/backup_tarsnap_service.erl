%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% @doc Tarsnap command/service related functions

-module(backup_tarsnap_service).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

-export([
    check_configuration/1,
    cleanup_before_backup/0,
    archives/1,
    archive_data/2,
    store/2,
    remove/1
]).
  
check_configuration(Context) ->
    Db = which(db_dump_cmd()),
    Tar = which(archive_cmd()),
    Tarsnap = which(tarsnap_cmd()),
    TarsnapConfigured = check_configuration_tarsnap(Context),
    [
        {ok, Db and Tar and Tarsnap and TarsnapConfigured},
        {db_dump, Db},
        {archive, Tar},
        {tarsnap, Tarsnap},
        {tarsnap_cfg, TarsnapConfigured}
    ].
     

cleanup_before_backup() ->
    Cmd = "tarsnap --fsck",
    os:cmd(Cmd).

   
%% Returns a list of archive names
archives(Context) ->
    Cfg = backup_tarsnap_service:check_configuration(Context),
    case proplists:get_value(ok, Cfg) of
        true ->
            Cmd = "tarsnap --list-archives",
            Result = os:cmd(Cmd),
            string:tokens(Result, "\n");
        false -> 
            ?zWarning("Problem getting the list of archives. Tarsnap may be busy. If this error continues, Tarsnap is either not installed or properly configured.", Context),
            []
    end.


%% Takes a list of archive names and returns the parsed data.
archive_data(Archives, Context) ->
    Identifier = backup_tarsnap_archive:identifier(Context),
    ArchiveData = backup_tarsnap_archive:parse_archive_names(Archives, Identifier),
    backup_tarsnap_cache:put(ArchiveData, Context),
    ArchiveData.

 
%% Store file as named archive.
store(Name, Path) ->
    % options:
    % -c: Create an archive containing the specified items and name.
    % -n: Do not recursively archive the contents of directories.
    % -f: Archive name
    TarsnapCmd = "tarsnap -c -n -f " ++ Name ++ " " ++ Path,
    os:cmd(TarsnapCmd).


%% Remove archive.
remove(Name) ->
    % options:
    % -d: Delete the specified archive
    % -f: Archive name
    TarsnapCmd = "tarsnap -d -f " ++ Name,
    os:cmd(TarsnapCmd).
 

%% If tarsnap is configured properly, tarsnap will return a message containing the table with All archives.
check_configuration_tarsnap(Context) ->
    ProcessingDir = z_path:files_subdir_ensure("processing", Context),
    Cmd = "tarsnap -v -c -f test --dry-run " ++ ProcessingDir,
    Result = os:cmd(Cmd),
    case re:run(Result, "(Removing leading)|(\\na [a-z0-9]+)|(All archives)|(Transaction already in progress)") of 
        {match, _Match} -> 
            true;
        _ -> 
            false
    end.


archive_cmd() ->
    z_convert:to_list(z_config:get(tar, "tar")).

db_dump_cmd() ->
    z_convert:to_list(z_config:get(pg_dump, "pg_dump")).
    
tarsnap_cmd() ->
    z_convert:to_list(z_config:get(tarsnap, "tarsnap")).

which(Cmd) ->
    filelib:is_regular(z_string:trim_right(os:cmd("which " ++ z_utils:os_escape(Cmd)))).
