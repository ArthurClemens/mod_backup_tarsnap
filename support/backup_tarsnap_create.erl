%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% Generated on 2014-10-31
%% @doc Tarsnap creation of archives

-module(backup_tarsnap_create).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    backup/1,
    backup/3
]).

backup(Context) ->
    Archives = backup_tarsnap_service:archives(Context),
    backup(Archives, [], Context).


backup(Archives, Options, Context) ->
    Cfg = backup_tarsnap_service:check_configuration(Context),
    case Cfg of
        ok -> 
            Identifier = backup_tarsnap_archive:identifier(Context),
            ArchiveData = backup_tarsnap_archive:parse_archive_names(Archives, Identifier),
            backup_tarsnap_cache:put(ArchiveData, Context),
            % handle jobs
            lists:map(fun(Job) ->
                JobArchives = [JobData || JobData <- ArchiveData, proplists:get_value(job, JobData) =:= Job],
                maybe_backup_for_job(Job, JobArchives, Options, Context)
            end, backup_tarsnap_job:jobs());
        Errors ->
            Msg = string:join(Errors, ", "),
            mod_backup_tarsnap:broadcast_error(Msg, Context)
    end.


maybe_backup_for_job(Job, JobArchives, Options, Context) when JobArchives =:= []->
    mod_backup_tarsnap:debug(io_lib:format("Backup job '~s':", [Job])),
    mod_backup_tarsnap:debug("No archives found. A new backup will be created."),
    {Name, TmpDir} = prepare_backup(Job, Context),
    case proplists:get_value(test, Options) of
        true ->
            mod_backup_tarsnap:debug(io_lib:format("A backup with name ~p will be created at path ~p", [Name, TmpDir])),
            mod_backup_tarsnap:debug("Test ends here."),
            undefined;
        _ ->
            do_backup(Name, TmpDir, Job, Context)
    end;

maybe_backup_for_job(Job, JobArchives, Options, Context) ->
    mod_backup_tarsnap:debug(io_lib:format("Backup job '~s':", [Job])),
    Sorted = lists:reverse(lists:sort(JobArchives)),
    [NewestArchive|_Rest] = Sorted,
    
    % take the smallest delta
    DeltaSeconds = backup_tarsnap_delta:smallest(Job, Context),
    Now = calendar:universal_time(),
    NowSeconds = calendar:datetime_to_gregorian_seconds(Now),
    NewestArchiveSeconds = backup_tarsnap_archive:date_seconds(NewestArchive),
    
    Diff = NewestArchiveSeconds + DeltaSeconds - NowSeconds,
    case (Diff < 0) of 
        true ->
            mod_backup_tarsnap:debug(io_lib:format("The smallest delta is set to ~p hours. The most recent archive ~p is ~p hours older. A new backup is needed.", [floor(DeltaSeconds/3600), proplists:get_value(archive, NewestArchive), abs(floor(Diff/3600))])),
            {Name, TmpDir} = prepare_backup(Job, Context),
            case proplists:get_value(test, Options) of
                true ->
                    mod_backup_tarsnap:debug(io_lib:format("A backup with name ~p will be created at path ~p", [Name, TmpDir])),
                    mod_backup_tarsnap:debug("Test ends here.");
                _ ->
                    do_backup(Name, TmpDir, Job, Context)
            end;
        false ->
            mod_backup_tarsnap:debug(io_lib:format("The smallest delta is set to ~p hours. The most recent archive ~p is ~p hours younger. No backup is needed.", [floor(DeltaSeconds/3600), proplists:get_value(archive, NewestArchive), abs(floor(Diff/3600))])),
            case proplists:get_value(test, Options) of
                true ->
                    mod_backup_tarsnap:debug("Test ends here.");
                _ ->
                    undefined
            end
    end.

  
prepare_backup(Job, Context) ->
    Name = backup_tarsnap_archive:name(Job, Context),
    TmpDir = z_path:files_subdir_ensure("processing", Context),
    {Name, TmpDir}.


do_backup(Name, TmpDir, Job, Context) ->
    Path = create_archive(Name, TmpDir, Job, Context),
    case Path of
        undefined ->
            z:warning(io_lib:format("Could not create backup ~s", [Job]), [{module, mod_backup_tarsnap}], Context);
        _ -> 
            _Result = backup_tarsnap_service:store(Name, Path),
            z:info(io_lib:format("Completed backup ~s", [Job]), [{module, mod_backup_tarsnap}], Context),
            file:delete(Path)
    end.


create_archive(Name, TmpDir, Job, Context) when Job =:= "database" ->
    backup_tarsnap_create_archive_database:create(Name, TmpDir, Context);
create_archive(Name, TmpDir, Job, Context) when Job =:= "files" ->
    backup_tarsnap_create_archive_files:create(Name, TmpDir, Context);
create_archive(_Name, _TmpDir, Job, Context) ->
    ?zWarning("Don't know how to backup job '~s'", [Job], Context).


floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.
