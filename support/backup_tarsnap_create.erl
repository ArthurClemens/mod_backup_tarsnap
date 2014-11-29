%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% @doc Tarsnap creation of archives

-module(backup_tarsnap_create).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    backup/1,
    backup/3
]).


-spec backup(Context) -> any() when
    Context:: #context{}.
backup(Context) ->
    Archives = backup_tarsnap_service:archives(Context),
    backup(Archives, [], Context).


-spec backup(Archives, Options, Context) -> any() when
    Archives:: list(),
    Options:: list({test, boolean()} | []),
    Context:: #context{}.
backup(Archives, Options, Context) ->
    ConfigOk = case proplists:get_value(test, Options, false) of
        true -> true;
        false -> 
            Cfg = backup_tarsnap_service:check_configuration(Context),
            backup_tarsnap_service:cleanup_before_backup(),
            proplists:get_value(ok, Cfg)
    end,
    lager:info("backup, Archives=~p", [Archives]),
    lager:info("ConfigOk=~p", [ConfigOk]),
    case ConfigOk of
        true ->
            ArchiveData = backup_tarsnap_service:archive_data(Archives, Context),
            lager:info("ArchiveData=~p", [ArchiveData]),
            % handle jobs
            lists:foreach(fun(Job) ->
                JobArchives = [JobData || JobData <- ArchiveData, proplists:get_value(job, JobData) =:= Job],
                maybe_backup_for_job(Job, JobArchives, Options, Context)
            end, backup_tarsnap_job:jobs());
        false ->
            mod_backup_tarsnap:debug("Tarsnap is not configured properly.", Context)
    end.


-spec maybe_backup_for_job(Job, JobArchives, Options, Context) -> any() when
    Job:: string(),
    JobArchives:: list(),
    Options:: list({test, boolean()} | []),
    Context:: #context{}.
maybe_backup_for_job(Job, JobArchives, Options, Context) when JobArchives =:= []->
    mod_backup_tarsnap:debug(io_lib:format("Assess backup job '~s'...", [Job]), Context),
    mod_backup_tarsnap:debug("No archives found. A new archive will be created.", Context),
    {Name, TmpDir} = prepare_backup(Job, Context),
    case proplists:get_value(test, Options, false) of
        true ->
            mod_backup_tarsnap:debug(io_lib:format("An archive with name ~p will be created at path ~p", [Name, TmpDir]), Context),
            mod_backup_tarsnap:debug("Test ends here.", Context);
        false ->
            do_backup(Name, TmpDir, Job, Context)
    end;

maybe_backup_for_job(Job, JobArchives, Options, Context) ->
    mod_backup_tarsnap:debug(io_lib:format("Assess backup job '~s'...", [Job]), Context),
    Sorted = lists:reverse(lists:sort(JobArchives)),
    [NewestArchive|_Rest] = Sorted,
    
    % take the smallest interval
    IntervalSeconds = backup_tarsnap_interval:smallest(Job, Context),
    NowSeconds = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    NewestArchiveSeconds = backup_tarsnap_archive:date_seconds(NewestArchive),    
    NextBackupSeconds = NewestArchiveSeconds + IntervalSeconds,
    Diff = NewestArchiveSeconds + IntervalSeconds - NowSeconds,
    
    case (NextBackupSeconds < NowSeconds) of 
        true ->        
            mod_backup_tarsnap:debug(io_lib:format("The smallest interval is set to ~.2f hours. The most recent archive ~p is ~.2f hours older. A new backup is needed.", [IntervalSeconds/3600, proplists:get_value(archive, NewestArchive), abs(Diff/3600)]), Context),
            {Name, TmpDir} = prepare_backup(Job, Context),
            case proplists:get_value(test, Options, false) of
                true ->
                    mod_backup_tarsnap:debug(io_lib:format("A archive with name ~p will be created at path ~p", [Name, TmpDir]), Context),
                    mod_backup_tarsnap:debug("Test ends here.", Context);
                false ->
                    do_backup(Name, TmpDir, Job, Context)
            end;
        false ->
            mod_backup_tarsnap:debug(io_lib:format("The smallest interval is set to ~.2f hours. The most recent archive ~p is ~.2f hours younger. No new backup is needed.", [IntervalSeconds/3600, proplists:get_value(archive, NewestArchive), abs(Diff/3600)]), Context),
            case proplists:get_value(test, Options, false) of
                true ->
                    mod_backup_tarsnap:debug("Test ends here.", Context);
                false ->
                    undefined
            end
    end.


-spec prepare_backup(Job, Context) -> {string(), string()} when
    Job:: string(),
    Context:: #context{}.
prepare_backup(Job, Context) ->
    Name = backup_tarsnap_archive:name(Job, Context),
    TmpDir = z_path:files_subdir_ensure("processing", Context),
    {Name, TmpDir}.


-spec do_backup(Name, TmpDir, Job, Context) -> any() when
    Name:: string(),
    TmpDir:: string(),
    Job:: string(),
    Context:: #context{}.
do_backup(Name, TmpDir, Job, Context) ->
    Path = create_archive(Name, TmpDir, Job, Context),
    case Path of
        undefined ->
            z:warning(io_lib:format("Could not create archive ~s", [Job]), [{module, mod_backup_tarsnap}], Context);
        _ -> 
            _Result = backup_tarsnap_service:store(Name, Path),
            z:info(io_lib:format("Completed backup ~s", [Job]), [{module, mod_backup_tarsnap}], Context),
            ok = file:delete(Path)
    end.


-spec create_archive(Name, TmpDir, Job, Context) -> list() | 'undefined' when
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
