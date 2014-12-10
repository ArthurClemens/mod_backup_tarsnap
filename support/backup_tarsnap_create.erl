%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% @doc Tarsnap creation of archives

-module(backup_tarsnap_create).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    backup/1,
    backup/2,
    
    % for testing:
    check_backup_needed/3,
    backup_name_dir/2
]).


-spec backup(Context) -> any() when
    Context:: #context{}.
backup(Context) ->
    Archives = backup_tarsnap_service:archives(Context),
    backup(Archives, Context).


-spec backup(Archives, Context) -> any() when
    Archives:: list(),
    Context:: #context{}.
backup(Archives, Context) ->
    Cfg = backup_tarsnap_service:check_configuration(Context),
    backup_tarsnap_service:cleanup_before_backup(),
    mod_backup_tarsnap:dev_debug("backup, Archives=~p", [Archives], Context),
    case proplists:get_value(ok, Cfg) of
        true ->
            ArchiveData = backup_tarsnap_service:archive_data(Archives, Context),
            mod_backup_tarsnap:dev_debug("ArchiveData=~p", [ArchiveData], Context),
            % handle jobs
            lists:foldl(fun(Job, Acc) ->
                JobArchives = [JobData || JobData <- ArchiveData, proplists:get_value(job, JobData) =:= Job],
                [maybe_backup_for_job(Job, JobArchives, Context)|Acc]
            end, [], backup_tarsnap_job:jobs());
        false ->
            mod_backup_tarsnap:debug("Tarsnap is not configured properly.", Context)
    end.


-spec maybe_backup_for_job(Job, JobArchives, Context) -> {error, string()} | {ok, string()} when
    Job:: string(),
    JobArchives:: list(),
    Context:: #context{}.
maybe_backup_for_job(Job, JobArchives, Context) when JobArchives =:= []->
    mod_backup_tarsnap:debug("No archives found. A new archive will be created.", Context),
    [Name, TmpDir] = backup_name_dir(Job, Context),
    do_backup(Name, TmpDir, Job, Context);
    
maybe_backup_for_job(Job, JobArchives, Context) ->
    mod_backup_tarsnap:debug(io_lib:format("Assess backup job '~s'...", [Job]), Context),
    [Name, TmpDir] = backup_name_dir(Job, Context),
    case check_backup_needed(Job, JobArchives, Context) of
        true ->
            do_backup(Name, TmpDir, Job, Context);
        false ->
            {ok, "No backup needed"}
    end.
    

-spec check_backup_needed(Job, JobArchives, Context) -> boolean() when
    Job:: string(),
    JobArchives:: list(),
    Context:: #context{}.
check_backup_needed(Job, JobArchives, Context) ->
    Sorted = lists:reverse(lists:sort(JobArchives)),
    [NewestArchive|_Rest] = Sorted,
    
    % take the smallest interval
    {_, IntervalSeconds0} = backup_tarsnap_interval:smallest(Job, Context),
    IntervalSeconds = round(IntervalSeconds0),
    NowSeconds = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    NewestArchiveSeconds = backup_tarsnap_archive:date_seconds(NewestArchive),    
    NextBackupSeconds = NewestArchiveSeconds + IntervalSeconds,
    Diff = NewestArchiveSeconds + IntervalSeconds - NowSeconds,
    case (NextBackupSeconds < NowSeconds) of 
        true ->
            mod_backup_tarsnap:debug(io_lib:format("The smallest interval is set to ~.2f hours. The most recent archive ~p is ~.2f hours older. A new backup is needed.", [IntervalSeconds/3600, proplists:get_value(archive, NewestArchive), abs(Diff/3600)]), Context),
            true;
        false ->
            mod_backup_tarsnap:debug(io_lib:format("The smallest interval is set to ~.2f hours. The most recent archive ~p is ~.2f hours younger. No new backup is needed.", [IntervalSeconds/3600, proplists:get_value(archive, NewestArchive), abs(Diff/3600)]), Context),
            false
    end.


-spec backup_name_dir(Job, Context) -> list() when
    Job:: string(),
    Context:: #context{}.
backup_name_dir(Job, Context) ->
    Name = backup_tarsnap_archive:name(Job, Context),
    TmpDir = z_path:files_subdir_ensure("processing", Context),
    [Name, TmpDir].


-spec do_backup(Name, TmpDir, Job, Context) -> {error, string()} | {ok, string()} when
    Name:: string(),
    TmpDir:: string(),
    Job:: string(),
    Context:: #context{}.
do_backup(Name, TmpDir, Job, Context) ->
    Path = create_archive(Name, TmpDir, Job, Context),
    case Path of
        undefined ->
            z:warning(io_lib:format("Could not create archive ~s", [Job]), [{module, mod_backup_tarsnap}], Context),
            {error, "Could not create archive"};
        _ -> 
            Result = backup_tarsnap_service:store(Name, Path),
            z:info(io_lib:format("Completed backup ~s", [Job]), [{module, mod_backup_tarsnap}], Context),
            ok = file:delete(Path),
            {ok, Result}
    end.


-spec create_archive(Name, TmpDir, Job, Context) -> undefined | list() when
    Name:: string(),
    TmpDir:: string(),
    Job:: string(),
    Context:: #context{}.
create_archive(Name, TmpDir, Job, Context) when Job =:= "database" ->
    backup_tarsnap_create_archive_database:create(Name, TmpDir, Context);
create_archive(Name, TmpDir, Job, Context) when Job =:= "files" ->
    backup_tarsnap_create_archive_files:create(Name, TmpDir, Context);
create_archive(_Name, _TmpDir, Job, Context) ->
    ?zWarning("Don't know how to backup job '~s'", [Job], Context).
