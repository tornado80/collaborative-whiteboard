-module(websocket_event_parser).

-include("event_payloads_records.hrl").

-export([
    parse/1,
    event_to_json/1
]).

parse(Json) ->
    case jsone:try_decode(Json, [{object_format, proplist}]) of
        {ok, PropList, _Remaining} ->
            parse_proplist(PropList);
        {error, _} -> 
            {error, <<"Invalid JSON">>}
    end.

parse_proplist(PropList) ->
    case proplists:get_value(<<"eventType">>, PropList) of
        undefined -> 
            {error, <<"eventType is not provided">>};
        EventType ->
            parse_event(EventType, PropList)
    end.

parse_event(<<"begin">>, PropList) ->
    {ok, #event{
        eventType = 'begin',
        eventId = proplists:get_value(<<"eventId">>, PropList, undefined),
        eventContent = #begin_payload{
            sessionType = case proplists:get_value(<<"sessionType">>, PropList, undefined) of
                undefined -> undefined;
                <<"new">> -> new;
                <<"continue">> -> continue;
                _ -> undefined
            end,
            sessionToken = proplists:get_value(<<"sessionToken">>, PropList, undefined),
            lastEventId = proplists:get_value(<<"lastEventId">>, PropList, undefined)
        }
    }};
parse_event(<<"reservationProposed">>, PropList) ->
    {ok, #event{
        eventType = reservationProposed,
        eventId = proplists:get_value(<<"eventId">>, PropList, undefined),
        eventContent = #reservation_proposed_payload{
            canvasObjectId = proplists:get_value(<<"canvasObjectId">>, PropList, undefined),
            proposalId = proplists:get_value(<<"proposalId">>, PropList, undefined)
        }
    }};
parse_event(<<"boardUpdateProposed">>, PropList) ->
    {ok, #event{
        eventType = boardUpdateProposed,
        eventId = proplists:get_value(<<"eventId">>, PropList, undefined),
        eventContent = #board_update_proposed_payload{
            proposalId = proplists:get_value(<<"proposalId">>, PropList, undefined),
            update = proplists:get_value(<<"update">>, PropList, undefined), % TODO: What to do parse and event_to_json or just proplist?
            intermediate = proplists:get_value(<<"intermediate">>, PropList, undefined)
        }
    }};
parse_event(<<"reservationCancellationRequested">>, PropList) ->
    {ok, #event{
        eventType = reservationCancellationRequested,
        eventId = proplists:get_value(<<"eventId">>, PropList, undefined),
        eventContent = #reservation_cancellation_requested_payload{
            reservationId = proplists:get_value(<<"reservationId">>, PropList, undefined)
        }
    }}.

-spec event_to_json(#event{}) -> binary().
event_to_json(#event{eventId = EventId, eventType = EventType, eventContent = Record}) ->
    jsone:encode([{<<"eventId">>, EventId}, {<<"eventType">>, EventType} | payload_to_proplist(Record)]).

payload_to_proplist(#welcome_user_payload{userId = UserId, sessionToken = SessionToken}) ->
    [{<<"userId">>, UserId}, {<<"sessionToken">>, SessionToken}].
