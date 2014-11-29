%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% @doc Archive related functions

-module(backup_tarsnap_archive).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    identifier/1,
    name/2,
    parse_archive_names/2,
    date_seconds/1,
    archive_names/1
]).


-spec identifier(Context) -> string() when
    Context:: #context{}.
%% Use z_context:site or optionally config 'identifier'.
identifier(Context) ->
    Identifier = m_config:get_value(
        mod_backup_tarsnap,
        identifier,
        list_to_binary(atom_to_list(z_context:site(Context))),
        Context
    ),
    case Identifier of
        <<>> -> atom_to_list(z_context:site(Context));
        _ -> binary_to_list(Identifier)
    end.


-spec name(JobName, Context) -> string() when
    JobName:: string(),
    Context:: #context{}.
%% Uses the same filename structure as mod_backup: id-dddddd-tttttt
%% Takes the identifier and appends the current date-time.
name(JobName, Context) ->
    DateStr = qdate:to_string("Ymd-His", calendar:universal_time()),
    identifier(Context)
        ++ "-"
        ++ JobName
        ++ "-"
        ++ DateStr.


-spec parse_archive_names(Archives, Identifier) -> list(string()) when
    Archives:: list(),
    Identifier:: string().
parse_archive_names(Archives, Identifier) ->
    lists:reverse(lists:foldl(fun(Archive, Acc) ->
        Data = parse_archive_parts(Identifier, Archive),
        case Data of
            [] -> Acc;
            _ ->
                [Data|Acc]
        end
    end, [], Archives)).


-spec date_seconds(ArchiveData) -> integer() when
    ArchiveData:: list().
%% Returns the number of seconds of the archive date.
date_seconds(ArchiveData) ->
    Date = proplists:get_value(date, ArchiveData),
    calendar:datetime_to_gregorian_seconds(Date).


-spec parse_archive_parts(Identifier, Archive) -> list() when
    Identifier:: string(),
    Archive:: string().
%% parses the date and extension from an archive name
%% parses the date from the date string
%% and returns a proplist with keys: date, extension
parse_archive_parts(Identifier, Archive) ->
    Pattern = 
        "^" ++
        Identifier ++
        "\-" ++ 
        "([a-z]+)"
        "\-" ++
        "([0-9]{4})([0-9]{2})([0-9]{2})\-([0-9]{2})([0-9]{2})([0-9]{2})" ++
        "$",
    case re:run(Archive, Pattern, [global,{capture,all,list}]) of 
        {match, [[_Match, Job, Year, Mon, Day, Hour, Min, Sec]]} -> 
            [
                {date, {{
                    list_to_integer(Year),
                    list_to_integer(Mon),
                    list_to_integer(Day)
                },{
                    list_to_integer(Hour),
                    list_to_integer(Min),
                    list_to_integer(Sec)
                }}},
                {job, Job},
                {archive, Archive}
            ];
        _ -> 
            []
    end.


-spec archive_names(Archives) -> list() when
    Archives:: list().
archive_names(Archives) ->
    lists:map(fun(Archive) ->
        proplists:get_value(archive, Archive)
    end, Archives).
