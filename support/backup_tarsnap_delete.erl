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

-define(ST_JUTTEMIS_SECONDS, calendar:datetime_to_gregorian_seconds({{9999,8,17}, {12,0,0}})).

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
    Intervals = backup_tarsnap_interval:intervals(Job, Context),
    IntervalData = create_interval_data(Intervals),    
    Buckets = distribute_archives_to_buckets(JobArchives, IntervalData),
    debug_buckets(Buckets, Context),
    Buckets1 = prune_buckets(Buckets),
    ToKeep = bucket_archives(Buckets1),
    ToRemove = JobArchives -- ToKeep,
    debug_result("To keep", ToKeep, Context),
    debug_result("To remove", ToRemove, Context),
    [{to_keep, ToKeep}, {to_remove, ToRemove}].


create_interval_data(Intervals) ->
    [_|Durations] = Intervals,
    Combined = lists:zip(Intervals, Durations ++ [{x, undefined}]),
    {IntervalData, _} = lists:foldl(fun({{Key, IntervalSeconds}, {_, Duration}}, {AccData, StartDate}) ->
        Count = case Duration of
            undefined -> undefined;
            _ -> round(Duration / IntervalSeconds) - 1 % subtract 1 because we will have next gens
        end,
        EndDate = case Duration of
            undefined -> ?ST_JUTTEMIS_SECONDS;
            _ -> StartDate + Duration
        end,
        Data = [
            {key, Key},
            {start_date, StartDate},
            {end_date, EndDate},
            {count, Count},
            {distribution, IntervalSeconds}
        ],
        {[Data|AccData], EndDate}
    end, {[], 0}, Combined),
    lists:reverse(IntervalData).


distribute_archives_to_buckets(Archives, IntervalData) ->
    SortedArchives = lists:sort(fun sort_by_date/2, Archives),
    MostRecentArchive = lists:nth(1, SortedArchives),
    MostRecentArchiveDate = proplists:get_value(date_seconds, MostRecentArchive),    
    {BucketData, _} = lists:foldl(fun(Interval, {Buckets, RemainingArchives}) ->
        % dates relative to most recent archive, backward in time
        BucketStart = MostRecentArchiveDate - proplists:get_value(start_date, Interval),
        BucketEnd = MostRecentArchiveDate - proplists:get_value(end_date, Interval),
        MatchingArchives = lists:filter(fun(A) ->
            ArchiveDate = proplists:get_value(date_seconds, A),
            (ArchiveDate =< BucketStart) and (ArchiveDate > BucketEnd)
        end, RemainingArchives),
        Bucket = [{archives, MatchingArchives}|Interval],
        {[Bucket|Buckets], RemainingArchives -- MatchingArchives}
    end, {[], SortedArchives}, IntervalData),
    lists:sort(fun sort_by_start_date/2, BucketData).


prune_buckets(Buckets) ->
    Pruned = lists:foldl(fun(Bucket, Acc) ->
        Archives = proplists:get_value(archives, Bucket),
        DistributionInterval = proplists:get_value(distribution, Bucket),
        Sorted = lists:sort(fun sort_by_date/2, Archives),
        Keep = case Sorted of 
            [] -> [[]];
            [A] -> [A];
            _ ->
                % keep the newest item (next gen)
                % then process the rest from old to new
                % first write out between intervals before and after
                Combined = lists:zip3(
                    [[]] ++ Sorted ++ [[]], % orginal
                    [[]] ++ [[]] ++ Sorted, % before
                    Sorted ++ [[]] ++ [[]] % after
                ),
                WithBeforeAfter = lists:foldl(fun({O, B, A}, Acc1) ->
                    case O of
                        [] -> Acc1;
                        _ -> 
                            OriginalDate = proplists:get_value(date_seconds, O),
                            BeforeDate = proplists:get_value(date_seconds, B),
                            AfterDate = proplists:get_value(date_seconds, A),
                            BeforeProp = case (OriginalDate =/= undefined) and (BeforeDate =/= undefined) of
                                false -> [];
                                true -> [{interval_before, abs(OriginalDate - BeforeDate)}]
                            end,
                            AfterProp = case (OriginalDate =/= undefined) and (AfterDate =/= undefined) of
                                false -> [];
                                true -> [{interval_after, abs(OriginalDate - AfterDate)}]
                            end,
                            [BeforeProp ++ AfterProp ++ O|Acc1]
                    end
                end, [], Combined),
                [First|Rest] = lists:sort(fun sort_by_date/2, WithBeforeAfter),
                {Remaining, _} = lists:foldl(fun(A, {Acc2, LastDate}) ->
                    Date = proplists:get_value(date_seconds, A),
                    case LastDate of
                        undefined ->
                            % keep the last item
                            {[clean_interval_props(A)|Acc2], Date};
                        _ -> 
                            % allow for a little bit of randomized intervals
                            MaxInterval = DistributionInterval * 0.9,
                            Before = proplists:get_value(interval_before, A, ?ST_JUTTEMIS_SECONDS),
                            After = proplists:get_value(interval_after, A, ?ST_JUTTEMIS_SECONDS),
                            case
                                (Before < MaxInterval) and
                                (After < MaxInterval) and
                                (((Before + After) / 2) < MaxInterval)
                            of
                                true ->
                                    {Acc2, Date};
                                false ->
                                    {[clean_interval_props(A)|Acc2], Date}
                            end
                    end
                end, {[clean_interval_props(First)], undefined}, lists:reverse(Rest)),
                Remaining
        end,
        [[{archives, Keep}|proplists:delete(archives, Bucket)]|Acc]
    end, [], Buckets),
    Pruned.


clean_interval_props(List) ->
    lists:foldl(fun(P, List1) ->
        proplists:delete(P, List1)
    end, List, [interval_before, interval_after]).


bucket_archives(Buckets) ->
    lists:foldl(fun(B, Acc) ->
        case proplists:get_value(archives, B) of
            [[]] -> Acc;
            A -> A ++ Acc
        end
    end, [], Buckets).
    

-spec do_delete(Names, Context) -> list() when
    Names:: list(),
    Context:: #context{}.
do_delete(Names, Context) -> 
    mod_backup_tarsnap:debug(io_lib:format("These archives will be removed: ~p", [Names]), Context),
    lists:map(fun(Name) ->
        backup_tarsnap_service:remove(Name),
        timer:sleep(1)
    end, Names).


% sort newest first
sort_by_date(A, B) ->
    proplists:get_value(date, B) =< proplists:get_value(date, A).


sort_by_start_date(A, B) ->
    proplists:get_value(start_date, A) =< proplists:get_value(start_date, B).


debug_buckets(BucketData, Context) ->
    mod_backup_tarsnap:dev_debug("BucketData: key: start day - end day (count x interval):", Context),
    lists:map(fun(Bucket) ->
        Key = proplists:get_value(key, Bucket),
        StartDate = proplists:get_value(start_date, Bucket),
        Distribution = proplists:get_value(distribution, Bucket),
        EndDate = proplists:get_value(end_date, Bucket),
        Count = proplists:get_value(count, Bucket),
        Archives = proplists:get_value(archives, Bucket),
        DaySeconds = 24 * 3600,
        mod_backup_tarsnap:dev_debug("\t ~p: ~p - ~p (~p x ~p)", [Key, StartDate/DaySeconds, EndDate/DaySeconds, Count, Distribution/DaySeconds], Context),
        case Archives of
            [] -> mod_backup_tarsnap:dev_debug("\t\t no archives", Context);
            _ -> 
                lists:map(fun(A) -> 
                    mod_backup_tarsnap:dev_debug("\t\t ~p", [A], Context)
                end, Archives)
        end
    end, BucketData).


debug_result(Title, Archives, Context) ->
    mod_backup_tarsnap:dev_debug("---", Context),
    mod_backup_tarsnap:dev_debug(Title, Context),
    lists:map(fun(A) ->
        mod_backup_tarsnap:dev_debug("\t ~p", [A], Context)
    end, lists:sort(fun sort_by_date/2, Archives)).


    