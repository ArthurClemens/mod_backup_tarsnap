%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% @doc Interval time spans

-module(backup_tarsnap_interval).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    smallest/2,
    intervals/2
]).

-define(DEFAULT_DELTA_LIST, <<"1d 1w 1m 1y">>).


-spec smallest(Job, Context) -> integer() when
    Job:: string(),
    Context:: #context{}.
smallest(Job, Context) ->
    lists:nth(1, lists:sort(intervals(Job, Context))).


-spec intervals(Job, Context) -> list() when
    Job:: string(),
    Context:: #context{}.
intervals(Job, Context) ->
    IntervalString = case m_config:get_value(
        mod_backup_tarsnap,
        list_to_atom(atom_to_list(interval) ++ "_" ++ Job), 
        m_config:get_value(
            mod_backup_tarsnap,
            interval,
            ?DEFAULT_DELTA_LIST,
            Context
        ),
        Context) of
            <<>> -> ?DEFAULT_DELTA_LIST;
            Intervals -> Intervals
    end,
    Seconds = delta_strings_to_seconds(binary_to_list(IntervalString)),
    lists:filter(fun(S) ->
        S > 0
    end, Seconds).


-spec delta_strings_to_seconds(IntervalString) -> list(integer()) when
    IntervalString:: list(binary()).
delta_strings_to_seconds(IntervalString) ->
    IntervalTokens = string:tokens(IntervalString, " "),
    Pattern = "(\\d+)(h|d|w|m|y)*",
    lists:map(fun(Interval) ->
        case re:run(Interval, Pattern, [global,{capture,all,list}]) of 
            {match, [[_Match, CountStr]]} -> 
                Duration = list_to_integer(CountStr),
                MinimumDuration = 10 * 60,
                case Duration < MinimumDuration of
                    true -> MinimumDuration;
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
                    _   -> 0
                end;
            _ -> 0
        end
    end, IntervalTokens).

