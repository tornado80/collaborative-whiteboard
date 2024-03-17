-module(websocket_event_parser).

-include("records.hrl").

-export([
    parse/1
]).

parse(Event) -> 
    case jsone:try_decode(Event, [{object_format, proplist}]) of
        {ok, Json, _Remaining} -> 
            parse_proplist(Json);
        {error, _} -> 
            {error, <<"Invalid JSON">>}
    end.

parse_proplist(List) ->
    case proplists:get_value(<<"eventType">>, List) of 
        undefined -> 
            {error, <<"eventType is not provided">>};
        EventType ->
            parse_event(EventType, List)
    end.

parse_event(<<"begin">>, List) ->
    {ok, #event{
        eventType = 'begin',
        eventId = proplists:get_value(<<"eventId">>, List, undefined),
        eventContent = #begin_event{
            sessionType = case proplists:get_value(<<"sessionType">>, List, undefined) of
                undefined -> undefined;
                <<"new">> -> new;
                <<"continue">> -> continue;
                _ -> undefined
            end,
            sessionToken = proplists:get_value(<<"sessionToken">>, List, undefined),
            lastEventId = proplists:get_value(<<"lastEventId">>, List, undefined)
        }
    }}.