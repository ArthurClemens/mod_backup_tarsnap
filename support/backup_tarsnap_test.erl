%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% @doc Test creating and deleting of archives

-module(backup_tarsnap_test).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    backup/1,
    delete/1
]).

backup(Context) ->
    backup_tarsnap_create:backup(create_test_archives(Context), [
        {test, true}
    ], Context).

delete(Context) ->
    backup_tarsnap_delete:delete(create_test_archives(Context), [
        {test, true},
        {date, {{2014,12,31},{22,45,0}}}
    ], Context).

create_test_archives(Context) ->
    TestDates = [
        "20141103-220040",
        "20141102-193700",
        "20141101-120500",
        "20141101-064500",
        "20141031-235500",
        "20141031-005500",
        "20141030-230000",
        "20141030-230100",
        "20141030-200000",
        "20141027-010000",
        "20141027-004500",
        "20141026-224500",
        "20141026-211000",
        "20141020-081000",
        "20141019-071000",
        "20141007-091500",
        "20140903-020400",
        "20140901-040400",
        "20140830-040100",
        "20140815-040100",
        "20140701-040100",
        "20140620-040100",
        "20140531-040100",
        "20140506-040100",
        "20140331-040100",
        "20140131-040100",
        "20131228-040100",
        "20131101-040100",
        "20131023-040100",
        "20130823-040100",
        "20120806-040100",
        "20120306-040100",
        "20110906-040100",
        "20110306-040100"
    ],
    Identifier = backup_tarsnap_archive:identifier(Context),
    Jobs = backup_tarsnap_job:jobs(),
    Archives = lists:foldl(fun(Date, Acc) ->
        lists:foldl(fun(Job, Acc1) ->
            NameData = Identifier ++ "-" ++ Job ++ "-" ++ Date,
            [[NameData] | Acc1]
        end, Acc, Jobs)
    end, [], TestDates),
    Archives1 = lists:concat(Archives),
    Archives1.
