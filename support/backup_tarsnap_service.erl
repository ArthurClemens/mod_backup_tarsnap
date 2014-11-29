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


-spec check_configuration(Context) -> list() when
    Context:: #context{}.
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
     

-spec cleanup_before_backup() -> string().
cleanup_before_backup() ->
    Cmd = "tarsnap --fsck",
    os:cmd(Cmd).


-spec archives(Context) -> list(string()) when
    Context:: #context{}.
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


-spec archive_data(Archives, Context) -> list() when
    Archives:: list(string()),
    Context:: #context{}.
%% Takes a list of archive names and returns the parsed data.
archive_data(Archives, Context) ->
    Identifier = backup_tarsnap_archive:identifier(Context),
    ArchiveData = backup_tarsnap_archive:parse_archive_names(Archives, Identifier),
    backup_tarsnap_cache:put(ArchiveData, Context),
    ArchiveData.

 
-spec store(Name, Path) -> string() when
    Name:: string(),
    Path:: string().
%% Store file as named archive.
store(Name, Path) ->
    % options:
    % -c: Create an archive containing the specified items and name.
    % -n: Do not recursively archive the contents of directories.
    % -f: Archive name
    TarsnapCmd = "tarsnap -c -n -f " ++ Name ++ " " ++ Path,
    os:cmd(TarsnapCmd).


-spec remove(Name) -> string() when
    Name:: string().
%% Remove archive.
remove(Name) ->
    % options:
    % -d: Delete the specified archive
    % -f: Archive name
    TarsnapCmd = "tarsnap -d -f " ++ Name,
    os:cmd(TarsnapCmd).
 

-spec check_configuration_tarsnap(Context) -> boolean() when
    Context:: #context{}.
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


-spec archive_cmd() -> string().
archive_cmd() ->
    z_convert:to_list(z_config:get(tar, "tar")).


-spec db_dump_cmd() -> string().
db_dump_cmd() ->
    z_convert:to_list(z_config:get(pg_dump, "pg_dump")).


-spec tarsnap_cmd() -> string().
tarsnap_cmd() ->
    z_convert:to_list(z_config:get(tarsnap, "tarsnap")).


-spec which(Cmd) -> boolean() when
    Cmd:: string().
which(Cmd) ->
    filelib:is_regular(z_string:trim_right(os:cmd("which " ++ z_utils:os_escape(Cmd)))).
