%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% @doc Creates a files archive

-module(backup_tarsnap_create_archive_files).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    create/3
]).

-spec create(Name, TmpDir, Context) -> string() | 'undefined' when
    Name:: string(),
    TmpDir:: string(),
    Context:: #context{}.
%% Copied largely from mod_backup:archive
%% Returns archive path, if any.
create(Name, TmpDir, Context) ->
    ArchiveDir = z_path:media_archive(Context),
    case filelib:is_dir(ArchiveDir) of
        true ->
            DumpFile = filename:join([TmpDir, Name ++ ".tar.gz"]),
            Command = lists:flatten([
                                     archive_cmd(),
                                     " -c -z ",
                                     "-f '", DumpFile, "' ",
                                     "-C '", ArchiveDir, "' ",
                                     " ."]),
            [] = os:cmd(Command),
            DumpFile;
        false ->
            %% No files uploaded
            undefined
    end.


-spec archive_cmd() -> string().
archive_cmd() ->
    z_convert:to_list(z_config:get(tar, "tar")).
