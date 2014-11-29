%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% @doc Creates a database archive

-module(backup_tarsnap_create_archive_database).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    create/3
]).

-spec create(Name, TmpDir, Context) -> string() | 'undefined' when
    Name:: string(),
    TmpDir:: string(),
    Context:: #context{}.
%% Copied largely from mod_backup:pg_dump.
%% Returns archive path, if any.
create(Name, TmpDir, Context) ->
    All = z_db_pool:get_database_options(Context),
    Host = proplists:get_value(dbhost, All),
    Port = proplists:get_value(dbport, All),
    User = proplists:get_value(dbuser, All),
    Password = proplists:get_value(dbpassword, All),
    Database = proplists:get_value(dbdatabase, All),
    Schema = proplists:get_value(dbschema, All),
    
    DumpFile = filename:join([TmpDir, Name ++ ".sql"]),
    PgPass = filename:join([dir(Context), ".pgpass"]),
    ok = file:write_file(PgPass, z_convert:to_list(Host)
                                ++":"++z_convert:to_list(Port)
                                ++":"++z_convert:to_list(Database)
                                ++":"++z_convert:to_list(User)
                                ++":"++z_convert:to_list(Password)),
    ok = file:change_mode(PgPass, 8#00600),
    Command = [
               "PGPASSFILE='",PgPass,"' '",
               db_dump_cmd(),
               "' -h ", Host,
               " -p ", z_convert:to_list(Port),
               " -w ", 
               " -f '", DumpFile, "' ",
               " -U '", User, "' ",
               case z_utils:is_empty(Schema) of
                   true -> [];
                   false -> [" -n '", Schema, "' "]
               end,
               Database],

    Result = case os:cmd(binary_to_list(iolist_to_binary(Command))) of
                 [] ->
                     ok;
                 Output ->
                     ?zWarning(Output, Context),
                     z_session_manager:broadcast(#broadcast{type="error", message=Output, title="mod_backup", stay=false}, Context),
                     {error, Output}
             end,
    ok = file:delete(PgPass),
    case Result of
        ok -> DumpFile;
        _ -> undefined
    end.


-spec dir(Context) -> string() when
    Context:: #context{}.
%% @doc Return and ensure the backup directory
dir(Context) ->
    z_path:files_subdir_ensure(backup, Context).


-spec db_dump_cmd() -> string().
db_dump_cmd() ->
    z_convert:to_list(z_config:get(pg_dump, "pg_dump")).