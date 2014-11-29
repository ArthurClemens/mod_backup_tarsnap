%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% @doc Tarsnap deletion of archives

-module(backup_tarsnap_delete).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    delete/1,
    delete/2,
    
    % for testing:
    calculate_candidates/3
]).


-spec delete(Context) -> any() when
    Context:: #context{}.
delete(Context) ->
    Archives = backup_tarsnap_service:archives(Context),
    delete(Archives, Context).


-spec delete(Archives, Context) -> any() when
    Archives:: list(),
    Context:: #context{}.
delete(Archives, Context) ->
    Cfg = backup_tarsnap_service:check_configuration(Context),
    case proplists:get_value(ok, Cfg) of
        true ->
            ArchiveData = backup_tarsnap_service:archive_data(Archives, Context),
            % handle jobs
            lists:foreach(fun(Job) ->
                JobArchives = [JobData || JobData <- ArchiveData, proplists:get_value(job, JobData) =:= Job],
                maybe_delete_for_job(Job, JobArchives, Context)
            end, backup_tarsnap_job:jobs());
        false ->
            mod_backup_tarsnap:debug("Tarsnap is not configured properly.", Context)
    end.
    

-spec maybe_delete_for_job(Job, JobArchives, Context) -> any() when
    Job:: string(),
    JobArchives:: list(),
    Context:: #context{}.
maybe_delete_for_job(Job, JobArchives, Context) when JobArchives =:= [] ->
    mod_backup_tarsnap:debug(io_lib:format("Assess delete job '~s'...", [Job]), Context),
    mod_backup_tarsnap:debug("No archives found, nothing to delete.", Context);

maybe_delete_for_job(Job, JobArchives, Context) when length(JobArchives) =:= 1 ->
    mod_backup_tarsnap:debug(io_lib:format("Assess delete job '~s'...", [Job]), Context),
    mod_backup_tarsnap:debug("Only 1 archive exists, so nothing to delete.", Context);
    
maybe_delete_for_job(Job, JobArchives, Context) ->
    mod_backup_tarsnap:debug(io_lib:format("Assess delete job '~s'...", [Job]), Context),
    [{to_keep, _UniqueToKeep}, {to_remove, ToRemove}] = calculate_candidates(Job, JobArchives, Context),
    case length(ToRemove) of
        0 ->
            mod_backup_tarsnap:debug("No backups to remove.", Context);
        _ -> 
            ToRemoveNames = backup_tarsnap_archive:archive_names(ToRemove),
            do_delete(ToRemoveNames, Context)
    end.
    
    
-spec calculate_candidates(Job, JobArchives, Context) -> list(tuple()) when
    Job:: string(),
    JobArchives:: list(),
    Context:: #context{}.
calculate_candidates(Job, JobArchives, Context) ->
    % Add property 'index' in order of the dates (from new to old)
    % so we have a handle of items to remove
    IndexedArchives = index_archives(JobArchives),
    
    Now = calendar:universal_time(),
    NowSeconds = calendar:datetime_to_gregorian_seconds(Now),
    
    % reverse sort intervals
    % and remove the first interval
    % start with last cycle
    Intervals = lists:reverse(lists:sort(backup_tarsnap_interval:intervals(Job, Context))),
    
    {ToKeep, _} = lists:foldl(fun(Interval, {ToKeep, WorkingArchives}) ->
        TargetTime = NowSeconds - Interval,
        
        mod_backup_tarsnap:dev_debug("---", Context),
        mod_backup_tarsnap:dev_debug("TargetTime: ~p, Interval=~p, in days=~p", [calendar:gregorian_seconds_to_datetime(TargetTime), Interval, Interval/(24*3600)], Context),
        
        % trim down possible candidates by looking at a smaller time window
        % we use half an interval
        Candidates = prune_archives(TargetTime, Interval/2, WorkingArchives),
        
        mod_backup_tarsnap:dev_debug("Candidates:", Context),
        case length(Candidates) of
            0 -> mod_backup_tarsnap:dev_debug("\t no candidates", Context);
            _ ->
                lists:map(fun(C) ->
                    mod_backup_tarsnap:dev_debug("\t ~p", [C], Context)
                end, lists:sort(Candidates))
        end,
        
        % Number of existing steps in this cycle
        StepCount = get_step_count(TargetTime, Interval, Candidates),
        mod_backup_tarsnap:dev_debug("Number of existing steps in this cycle: ~p", [StepCount], Context),
        
        ClosestCandidates = lists:foldl(fun(Step, Acc1) ->
            GroupTargetTime = TargetTime - (Step * Interval),
            case find_closest_backup_in_range(GroupTargetTime, Interval/2, Candidates, Context) of 
                [] -> Acc1;
                Closest -> [Closest|Acc1]
            end
        end, [], lists:seq(0, StepCount - 1)),
        
        mod_backup_tarsnap:dev_debug("ClosestCandidates:", Context),
        case length(ClosestCandidates) of
            0 -> mod_backup_tarsnap:dev_debug("\t no closest candidates", Context);
            _ -> 
                lists:map(fun(C) ->
                    mod_backup_tarsnap:dev_debug("\t ~p", [C], Context)
                end, lists:sort(ClosestCandidates))
        end,
        
        {MarkAsKeep, CleanedWorking} = case ClosestCandidates of
            [] ->
                {[], WorkingArchives};
            _ -> 
                % add ClosestCandidates to "keep", then remove everything older  from WorkingArchives
                ClosestCandidatesOldestFirst = lists:reverse(lists:sort(fun sort_by_date/2, ClosestCandidates)),
                OldestClosest = lists:nth(1, ClosestCandidatesOldestFirst),
                OldestClosestIndex = proplists:get_value(index, OldestClosest),
                MaybeKeepLater = remove_from_list(OldestClosestIndex, WorkingArchives),
                
                mod_backup_tarsnap:dev_debug("MaybeKeepLater:", Context),
                lists:map(fun(C) ->
                    mod_backup_tarsnap:dev_debug("\t ~p", [C], Context)
                end, lists:sort(MaybeKeepLater)),
                
                {ClosestCandidates, MaybeKeepLater}
        end,
        {MarkAsKeep ++ ToKeep, CleanedWorking}
    end, {[lists:nth(1, IndexedArchives)], IndexedArchives}, Intervals),
    UniqueToKeep = lists:usort(ToKeep),
    ToRemove = IndexedArchives -- UniqueToKeep,
    
    mod_backup_tarsnap:dev_debug("To keep:", Context),
    lists:map(fun(C) ->
        mod_backup_tarsnap:dev_debug("\t ~p", [C], Context)
    end, lists:sort(ToKeep)),
    
    mod_backup_tarsnap:dev_debug("To remove:", Context),
    lists:map(fun(C) ->
        mod_backup_tarsnap:dev_debug("\t ~p", [C], Context)
    end, lists:sort(ToRemove)),
    
    [{to_keep, remove_index(UniqueToKeep)}, {to_remove, remove_index(ToRemove)}].


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
    

-spec remove_index(Archives) -> list() when
    Archives:: list().
remove_index(Archives) ->
    lists:map(fun(L) ->
        proplists:delete(index, L)
    end, Archives).


-spec prune_archives(Time, LookupInterval, Archives) -> list() when
    Time:: non_neg_integer(),
    LookupInterval:: float(),
    Archives:: list().
prune_archives(Time, LookupInterval, Archives) ->
    % include archives that are half a interval younger, plus all older
    Youngest = Time + LookupInterval,
    lists:filter(fun(Archive) ->
        Date = proplists:get_value(date, Archive),
        DateSeconds = calendar:datetime_to_gregorian_seconds(Date),
        DateSeconds =< Youngest
    end, Archives).
    

-spec get_step_count(Time, Interval, Archives) -> integer() when
    Time:: non_neg_integer(),
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


-spec find_closest_backup_in_range(Date, LookupInterval, Archives, Context) -> list() when
    Date:: integer(),
    LookupInterval:: float(),
    Archives:: list(),
    Context:: #context{}.
%% Find the closest matching date for given date.
%% Search in the range: Date +- Interval/2 
find_closest_backup_in_range(Date, LookupInterval, Archives, Context) ->
    mod_backup_tarsnap:dev_debug("find_closest_backup_in_range: ~p, interval=~p, in days=~p", [calendar:gregorian_seconds_to_datetime(Date), LookupInterval, LookupInterval/(24*3600)], Context),
    
    Diffed = lists:foldl(fun(Archive, Acc) ->
        ArchiveDate = proplists:get_value(date, Archive),
        ArchiveDateSeconds = calendar:datetime_to_gregorian_seconds(ArchiveDate),
        Diff = abs(Date - ArchiveDateSeconds),
        [[{diff, Diff}|Archive] | Acc]
    end, [], Archives),

    ValidDiffed = lists:filter(fun(D) ->
        proplists:get_value(diff, D) =< LookupInterval
    end, Diffed),
    
    mod_backup_tarsnap:dev_debug("ValidDiffed:", Context),
    lists:map(fun(C) ->
        mod_backup_tarsnap:dev_debug("\t ~p", [C], Context)
    end, lists:sort(ValidDiffed)),

    case ValidDiffed of
        [] -> [];
        _ -> 
            Sorted = lists:sort(fun sort_by_diff/2, ValidDiffed),
            % remove 'diff' property
            Sorted1 = lists:map(fun(Archive) ->
                proplists:delete(diff, Archive)
            end, Sorted),
            % return the first item
            lists:nth(1, Sorted1)
    end.


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