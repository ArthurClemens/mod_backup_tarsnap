%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% @doc Backup jobs: files and database

-module(backup_tarsnap_job).
-author("Arthur Clemens").

-export([
    jobs/0
]).

-define(JOBS, ["files", "database"]).

jobs() ->
    ?JOBS.
