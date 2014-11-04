%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% Generated on 2014-10-31
%% @doc Delta time spans

-module(backup_tarsnap_delta).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    smallest/2,
    deltas/2
]).

-define(DEFAULT_DELTA_LIST, <<"1d 1w 1m 1y">>).

smallest(Job, Context) ->
    lists:nth(1, lists:sort(deltas(Job, Context))).


deltas(Job, Context) ->
    DeltaString = case m_config:get_value(
        mod_backup_tarsnap,
        list_to_atom(atom_to_list(deltas) ++ "_" ++ Job), 
        m_config:get_value(
            mod_backup_tarsnap,
            deltas,
            ?DEFAULT_DELTA_LIST,
            Context
        ),
        Context) of
            <<>> -> ?DEFAULT_DELTA_LIST;
            Deltas -> Deltas
    end,
    delta_strings_to_seconds(binary_to_list(DeltaString)).


delta_strings_to_seconds(DeltaString) ->
    DeltaTokens = string:tokens(DeltaString, " "),
    Pattern = "(\\d+)(h|d|w|m|y)*",
    lists:map(fun(Delta) ->
        case re:run(Delta, Pattern, [global,{capture,all,list}]) of 
            {match, [[_Match, CountStr]]} -> 
                Duration = list_to_integer(CountStr),
                case Duration < 120 of
                    true -> 120;
                    false -> Duration
                end;
            {match, [[_Match, CountStr, Type]]} -> 
                Count = list_to_integer(CountStr),
                case Type of
                    "h" -> Count * 3600;
                    "d" -> Count * 3600 * 24;
                    "w" -> Count * 3600 * 24 * 7;
                    "m" -> Count * 3600 * 24 * 30;
                    "y" -> Count * 3600 * 24 * 365;
                    _   -> undefined
                end;
            _ -> undefined
        end
    end, DeltaTokens).

