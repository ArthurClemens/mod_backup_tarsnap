%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% @doc Tarsnap deletion of archives

-module(backup_tarsnap_delete).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    delete/1,
    delete/3
]).


-spec delete(Context) -> any() when
    Context:: #context{}.
delete(Context) ->
    Archives = backup_tarsnap_service:archives(Context),
    delete(Archives, [], Context).


-spec delete(Archives, Options, Context) -> any() when
    Archives:: list(),
    Options:: list({test, boolean()} | {date, tuple()} | []),
    Context:: #context{}.
delete(Archives, Options, Context) ->
    Cfg = backup_tarsnap_service:check_configuration(Context),
    case proplists:get_value(ok, Cfg) of
        true ->
            ArchiveData = backup_tarsnap_service:archive_data(Archives, Context),
            % handle jobs
            lists:foreach(fun(Job) ->
                JobArchives = [JobData || JobData <- ArchiveData, proplists:get_value(job, JobData) =:= Job],
                maybe_delete_for_job(Job, JobArchives, Options, Context)
            end, backup_tarsnap_job:jobs());
        false ->
            mod_backup_tarsnap:debug("Tarsnap is not configured properly.", Context)
    end.
    

-spec maybe_delete_for_job(Job, JobArchives, Options, Context) -> any() when
    Job:: string(),
    JobArchives:: list(),
    Options:: list({test, boolean()} | {date, tuple()} | []),
    Context:: #context{}.
maybe_delete_for_job(Job, JobArchives, _Options, Context) when JobArchives =:= [] ->
    mod_backup_tarsnap:debug(io_lib:format("Assess delete job '~s'...", [Job]), Context),
    mod_backup_tarsnap:debug("No archives found, nothing to delete.", Context);

maybe_delete_for_job(Job, JobArchives, _Options, Context) when length(JobArchives) =:= 1 ->
    mod_backup_tarsnap:debug(io_lib:format("Assess delete job '~s'...", [Job]), Context),
    mod_backup_tarsnap:debug("Only 1 archive exists, so nothing to delete.", Context);
    
maybe_delete_for_job(Job, JobArchives, Options, Context) ->
    mod_backup_tarsnap:debug(io_lib:format("Assess delete job '~s'...", [Job]), Context),
    
    % Add property 'index' in order of the dates (from new to old)
    % so we have a handle of items to remove
    IndexedArchives = index_archives(JobArchives),
    
    Now = proplists:get_value(date, Options, calendar:universal_time()),
    NowSeconds = calendar:datetime_to_gregorian_seconds(Now),
    
    % reverse sort intervals
    % and remove the first interval
    % start with last cycle
    Intervals = lists:reverse(lists:sort(backup_tarsnap_interval:intervals(Job, Context))),
    
    {ToKeep, _} = lists:foldl(fun(Interval, {ToKeep, WorkingArchives}) ->
        TargetTime = NowSeconds - Interval,
        %% trim down possible candidates by looking at a smaller time window
        Candidates = prune_archives(TargetTime, Interval, WorkingArchives),
        % Number of existing steps in this cycle
        StepCount = get_step_count(TargetTime, Interval, Candidates),
        RawStepArchives = lists:foldl(fun(Step, Acc1) ->
            GroupTargetTime = TargetTime - (Step * Interval),
            case find_closest_backup_in_range(GroupTargetTime, Interval, Candidates) of 
                undefined -> Acc1;
                Closest -> [Closest|Acc1]
            end
        end, [], lists:seq(0, StepCount - 1)),
        {ToAdd, CleanedWorking} = case RawStepArchives of
            [] -> {[], WorkingArchives};
            _ -> 
                StepArchives = lists:reverse(lists:sort(fun sort_by_date/2, lists:usort(RawStepArchives))),
                % add these to "keep", then remove everything older than newest from WorkingArchives
                NewestStepArchive = lists:nth(1, StepArchives),
                NewestStepArchiveIndex = proplists:get_value(index, NewestStepArchive),
                Cleaned = remove_from_list(NewestStepArchiveIndex, WorkingArchives),
                {StepArchives, Cleaned}
        end,            
        {ToAdd ++ ToKeep, CleanedWorking}
    end, {[lists:nth(1, IndexedArchives)], IndexedArchives}, Intervals),
    UniqueToKeep = lists:usort(ToKeep),
    ToRemove = IndexedArchives -- UniqueToKeep,
    case length(ToRemove) of
        0 ->
            case proplists:get_value(test, Options, false) of
                true ->
                    mod_backup_tarsnap:debug("No backups to remove.", Context),
                    mod_backup_tarsnap:debug(io_lib:format("~p backups to keep.", [length(UniqueToKeep)]), Context),
                    mod_backup_tarsnap:debug("Test ends here.", Context);
                false ->
                    mod_backup_tarsnap:debug("No backups to remove.", Context)
            end;
        _ -> 
            ToRemoveNames = backup_tarsnap_archive:archive_names(ToRemove),
            case proplists:get_value(test, Options, false) of
                true ->
                    ToKeepNames = backup_tarsnap_archive:archive_names(UniqueToKeep),
                    mod_backup_tarsnap:debug(io_lib:format("These archives will be kept: ~p", [ToKeepNames]), Context),
                    mod_backup_tarsnap:debug(io_lib:format("These archives will be removed: ~p", [ToRemoveNames]), Context),
                    mod_backup_tarsnap:debug("Test ends here.", Context);
                false ->
                    do_delete(ToRemoveNames, Context)
            end
    end.


-spec do_delete(Names, Context) -> list() when
    Names:: list(),
    Context:: #context{}.
do_delete(Names, Context) -> 
    mod_backup_tarsnap:debug(io_lib:format("These archives will be removed: ~p", [Names]), Context),
    lists:map(fun(Name) ->
        backup_tarsnap_service:remove(Name),
        timer:sleep(1)
    end, Names).


-spec index_archives(Archives) -> list() when
    Archives:: list().
index_archives(Archives) ->
    SortedArchives = lists:reverse(lists:sort(fun sort_by_date/2, Archives)),
    {_, Indexed} = lists:foldl(fun(Archive, {Counter, List}) ->
        IndexedArchive = [{index, Counter}|Archive],
        {Counter + 1, [IndexedArchive|List]}
    end, {1, []}, SortedArchives),
    lists:reverse(lists:sort(fun sort_by_date/2, Indexed)).
    

-spec prune_archives(Time, Interval, Archives) -> list() when
    Time:: integer(),
    Interval:: integer(),
    Archives:: list().
prune_archives(Time, Interval, Archives) ->
    % include archives that are half a interval younger, plus all older
    Youngest = Time + Interval/2,
    lists:filter(fun(Archive) ->
        Date = proplists:get_value(date, Archive),
        DateSeconds = calendar:datetime_to_gregorian_seconds(Date),
        DateSeconds =< Youngest
    end, Archives).
    

-spec get_step_count(Time, Interval, Archives) -> integer() when
    Time:: integer(),
    Interval:: integer(),
    Archives:: list().
get_step_count(_Time, _Interval, Archives) when length(Archives) =:= 0 ->
    0;
get_step_count(Time, Interval, Archives) ->
    SortedArchives = lists:sort(fun sort_by_date/2, Archives),
    OldestArchive = lists:nth(1, SortedArchives),
    OldestDate = proplists:get_value(date, OldestArchive),
    OldestSeconds = calendar:datetime_to_gregorian_seconds(OldestDate),
    StartTime = Time + Interval/2,
    ceiling((StartTime - OldestSeconds) / Interval).


-spec find_closest_backup_in_range(Date, Interval, Archives) -> integer() | 'undefined' when
    Date:: integer(),
    Interval:: integer(),
    Archives:: list().
%% Find the closest matching date for given date.
%% Search in the range: Date +- Interval/2 
find_closest_backup_in_range(Date, Interval, Archives) ->
    Diffed = lists:foldl(fun(Archive, Acc) ->
        ArchiveDate = proplists:get_value(date, Archive),
        ArchiveDateSeconds = calendar:datetime_to_gregorian_seconds(ArchiveDate),
        Diff = abs(Date - ArchiveDateSeconds),
        case Diff > Interval/2 of
            true -> Acc;
            false -> 
                Archive1 = [{diff, Diff}|Archive],
                [Archive1|Acc]
        end
    end, [], Archives),
    Closest = case Diffed of
        [] -> undefined;
        _ -> 
            Sorted = lists:reverse(lists:sort(fun sort_by_diff/2, Diffed)),
            Sorted1 = lists:map(fun(Archive) ->
                proplists:delete(diff, Archive)
            end, Sorted),
            lists:nth(1, Sorted1)
    end,
    Closest.


-spec remove_from_list(FromIndex, Archives) -> list() when
    FromIndex:: integer(),
    Archives:: list().
remove_from_list(FromIndex, Archives) ->
    lists:filter(fun(Archive) ->
        Index = proplists:get_value(index, Archive),
        Index < FromIndex
    end, Archives).

            
sort_by_date(A, B) ->
    proplists:get_value(date, A) =< proplists:get_value(date, B).

sort_by_diff(A, B) ->
    proplists:get_value(diff, A) =< proplists:get_value(diff, B).

-spec ceiling(X) -> integer() when
    X:: float().
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.