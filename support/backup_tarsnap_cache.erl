%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% @doc Stores slow calls to --list-archives for fast retrieval.

-module(backup_tarsnap_cache).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    init/1,
    put/2,
    get/1
]).


-spec init(Context) -> atom() when
    Context:: #context{}.
init(Context) ->
    case z_db:table_exists(mod_backup_tarsnap_cache, Context) of
        false ->
            z_db:create_table(mod_backup_tarsnap_cache, [
                #column_def{
                    name=archive,
                    type="text",
                    is_nullable=false
                },
                #column_def{
                    name=job,
                    type="text",
                    is_nullable=false
                },
                #column_def{
                    name=date,
                    type="timestamp",
                    is_nullable=false
                },
                #column_def{
                    name=status,
                    type="text",
                    is_nullable=true
                }
            ], Context);
        true -> ok
    end.


-spec put(ArchiveData, Context) -> [{'ok',_}] when
    ArchiveData:: list(),
    Context:: #context{}.
put(ArchiveData, Context) ->
    % empty and fill up again
    z_db:q("TRUNCATE mod_backup_tarsnap_cache", Context),
    lists:map(fun(Archive) ->
        Columns = [
            {archive, proplists:get_value(archive, Archive)},
            {job, proplists:get_value(job, Archive)},
            {date, proplists:get_value(date, Archive)},
            {status, <<>>}
        ],
        {ok, _} = z_db:insert(mod_backup_tarsnap_cache, Columns, Context)
    end, ArchiveData).


-spec get(Context) -> list() when
    Context:: #context{}.
get(Context) ->
    Query = "SELECT * FROM mod_backup_tarsnap_cache ORDER BY date desc",
    z_db:assoc(Query, Context).
    
    