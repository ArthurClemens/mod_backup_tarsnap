%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% @doc Test creating and deleting of archives

-module(backup_tarsnap_test).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    backup/2,
    delete/3
]).

-define(DAY_SECONDS, 3600 * 24).

-spec backup(Count, Context) -> any() when
    Count:: integer(),
    Context:: #context{}.
backup(Count, Context) ->
    CreateFun = fun days_back/1,
    Archives = case Count of
        0 -> [];
        _ -> create_test_archives(CreateFun, Count, Context)
    end,
    ArchiveData = backup_tarsnap_service:archive_data(Archives, Context),
    Job = "database",
    JobArchives = [JobData || JobData <- ArchiveData, proplists:get_value(job, JobData) =:= Job],
    mod_backup_tarsnap:dev_debug("ArchiveData=~p", [ArchiveData], Context),
    mod_backup_tarsnap:dev_debug("JobArchives=~p", [JobArchives], Context),
    case JobArchives of
        [] ->
            [Name, TmpDir] = backup_tarsnap_create:backup_name_dir(Job, Context),
            mod_backup_tarsnap:dev_debug("New backup: ~p, ~p", [Name, TmpDir], Context);
        _ -> 
            [Name, TmpDir] = backup_tarsnap_create:backup_name_dir(Job, Context),
            case backup_tarsnap_create:check_backup_needed(Job, JobArchives, Context) of
                true ->
                    mod_backup_tarsnap:dev_debug("New backup: ~p, ~p", [Name, TmpDir], Context);
                false ->
                    mod_backup_tarsnap:dev_debug("No backup", Context)
            end
    end.


-spec delete(Count, Repeat, Context) -> any() when
    Count:: integer(),
    Repeat:: integer(),
    Context:: #context{}.
delete(Count, Repeat, Context) ->
    CreateFun = fun days_back/1,
%    CreateFun = fun months_back/1,
    Archives = create_test_archives(CreateFun, Count, Context),
    ArchiveData = backup_tarsnap_service:archive_data(Archives, Context),
    Job = "database",
    JobArchives = [JobData || JobData <- ArchiveData, proplists:get_value(job, JobData) =:= Job],
    lists:foldl(fun(_L, JobArchives1) -> 
        [{to_keep, UniqueToKeep}, {to_remove, ToRemove}] = backup_tarsnap_delete:calculate_candidates(Job, JobArchives1, Context),
        mod_backup_tarsnap:dev_debug("To keep: ~p", [length(UniqueToKeep)], Context),
        lists:map(fun(A) ->
            mod_backup_tarsnap:dev_debug("\t ~p", [A], Context)
        end, lists:sort(UniqueToKeep)),
        mod_backup_tarsnap:dev_debug("To remove: ~p", [length(ToRemove)], Context),
        lists:map(fun(A) ->
            mod_backup_tarsnap:dev_debug("\t ~p", [A], Context)
        end, lists:sort(ToRemove)),
        mod_backup_tarsnap:dev_debug("-------", Context),
        UniqueToKeep
    end, JobArchives, lists:seq(1, Repeat)).


-spec create_test_archives(CreateFun, Count, Context) -> list() when
    CreateFun:: fun((_) -> list()),
    Count:: integer(),
    Context:: #context{}.
create_test_archives(CreateFun, Count, Context) ->
    TestDates = CreateFun(Count),
    Identifier = backup_tarsnap_archive:identifier(Context),
    Jobs = ["database"], %backup_tarsnap_job:jobs(),
    Archives = lists:foldl(fun(Date, Acc) ->
        lists:foldl(fun(Job, Acc1) ->
            NameData = Identifier ++ "-" ++ Job ++ "-" ++ Date,
            [[NameData]|Acc1]
        end, Acc, Jobs)
    end, [], TestDates),
    mod_backup_tarsnap:dev_debug("Test archives:", Context),
    lists:map(fun(A) ->
        mod_backup_tarsnap:dev_debug("\t ~p", [A], Context)
    end, lists:sort(Archives)),
    lists:concat(Archives).
   

-spec days_back(DayCount) -> list(non_neg_integer()) when
    DayCount:: non_neg_integer().
days_back(DayCount) ->
    Now = now_seconds(),
    lists:map(fun(D) -> 
        RandomRange = ?DAY_SECONDS / 0.8,
        Random = RandomRange - random:uniform(round(2 * RandomRange)),
        format_date(round(Now - (D * ?DAY_SECONDS) + Random))
    end, lists:seq(0, DayCount)).


%-spec months_back(MonthCount) -> list(non_neg_integer()) when
%    MonthCount:: non_neg_integer().
%months_back(MonthCount) ->
%    Now = now_seconds(),
%    lists:map(fun(D) -> 
%        RandomRange = ?DAY_SECONDS * 30 / 0.8,
%        Random = RandomRange - random:uniform(round(2 * RandomRange)),
%        format_date(round(Now - (D * 30.3 * ?DAY_SECONDS) + Random))
%    end, lists:seq(0, MonthCount)).


-spec now_seconds() -> integer().
now_seconds() ->
    Now = erlang:now(),
    qdate:to_unixtime(Now).

-spec format_date(Date) -> string() when
    Date:: integer().
format_date(Date) ->
    qdate:to_string("Ymd-His", Date).
