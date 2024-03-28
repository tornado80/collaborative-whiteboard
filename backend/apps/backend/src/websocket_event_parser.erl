-module(websocket_event_parser).

-include("event_payloads_records.hrl").

-export([
    json_to_event/1,
    event_to_json/1
]).

-spec json_to_event(binary()) -> {ok, #event{}} | {error, binary()}.
json_to_event(Json) ->
    case jsone:try_decode(Json, [{object_format, proplist}]) of
        {ok, PropList, _RemainingJson} ->
            proplist_to_event(PropList);
        {error, _} -> 
            {error, <<"Invalid JSON">>}
    end.

proplist_to_event(PropList) ->
    case proplists:get_value(<<"eventType">>, PropList) of
        undefined -> 
            {error, <<"eventType is not provided">>};
        EventType ->
            proplist_to_event(EventType, PropList)
    end.

proplist_to_event(<<"begin">>, PropList) ->
    {ok, #event{
        eventType = 'begin',
        eventPayload = #begin_payload{
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
proplist_to_event(<<"reservationProposed">>, PropList) ->
    {ok, #event{
        eventType = reservationProposed,
        eventPayload = #reservation_proposed_payload{
            canvasObjectId = proplists:get_value(<<"canvasObjectId">>, PropList, undefined),
            proposalId = proplists:get_value(<<"proposalId">>, PropList, undefined)
        }
    }};
proplist_to_event(<<"boardUpdateProposed">>, PropList) ->
    {ok, #event{
        eventType = boardUpdateProposed,
        eventPayload = #board_update_proposed_payload{
            proposalId = proplists:get_value(<<"proposalId">>, PropList, undefined),
            update = proplist_to_update_payload(proplists:get_value(<<"update">>, PropList, undefined)),
            intermediate = proplists:get_value(<<"intermediate">>, PropList, undefined)
        }
    }};
proplist_to_event(<<"reservationCancellationRequested">>, PropList) ->
    {ok, #event{
        eventType = reservationCancellationRequested,
        eventPayload = #reservation_cancellation_requested_payload{
            reservationId = proplists:get_value(<<"reservationId">>, PropList, undefined)
        }
    }};
proplist_to_event(<<"undoRequested">>, _PropList) ->
    {ok, #event{
        eventType = undoRequested,
        eventPayload = undefined
    }};
proplist_to_event(<<"redoRequested">>, _PropList) ->
    {ok, #event{
        eventType = redoRequested,
        eventPayload = undefined
    }};
proplist_to_event(EventType, _) ->
    {error, io_lib:format(<<"unexpected eventType">>, [EventType])}.

proplist_to_update_payload(PropList) ->
    #update_payload{
        canvasObjectId = proplists:get_value(<<"canvasObjectId">>, PropList, undefined),
        canvasObjectType = case proplists:get_value(<<"canvasObjectType">>, PropList, undefined) of
            undefined -> undefined;
            <<"stickyNote">> -> stickyNote;
            <<"image">> -> image;
            <<"comment">> -> comment;
            <<"canvas">> -> canvas;
            _ -> undefined
        end,
        operationType = case proplists:get_value(<<"operationType">>, PropList, undefined) of
            undefined -> undefined;
            <<"create">> -> create;
            <<"update">> -> update;
            <<"delete">> -> delete;
            <<"draw">> -> draw;
            <<"erase">> -> erase;
            _ -> undefined
        end,
        operation = proplist_to_canvas_object_operation(
            proplists:get_value(<<"operation">>, PropList, undefined))
    }.

proplist_to_canvas_object_operation(PropList) ->
    case proplists:get_value(<<"canvasObjectOperationType">>, PropList, undefined) of
        undefined -> undefined;
        CanvasObjectOperationType -> #canvas_object_operation{
            canvasObjectOperationType = CanvasObjectOperationType,
            canvasObjectOperationPayload = proplist_to_canvas_object_operation_payload(
                CanvasObjectOperationType, PropList)
        }
    end.

proplist_to_canvas_object_operation_payload(_CanvasObjectOperationType, PropList) ->
    PropList.

-spec event_to_json(#event{}) -> binary().
event_to_json(
    #event{
        eventType = EventType,
        eventPayload = EventPayload
    }) ->
    jsone:encode([
        {<<"eventType">>, EventType} | payload_to_proplist(EventPayload)]).

% server (reply) to client
payload_to_proplist(
    #welcome_user_payload{
        userId = UserId,
        sessionToken = SessionToken,
        userName = UserName,
        color = Color
    }) ->
    [{<<"userId">>, UserId}, {<<"sessionToken">>, SessionToken}, {<<"userName">>, UserName}, {<<"color">>, Color}];
payload_to_proplist(
    #reservation_proposal_succeeded_payload{
        proposalId = ProposalId,
        reservationId = ReservationId,
        expirationTimestamp = ExpirationTimestamp
    }) ->
    [
        {<<"proposalId">>, ProposalId},
        {<<"reservationId">>, ReservationId},
        {<<"expirationTimestamp">>, ExpirationTimestamp}
    ];
payload_to_proplist(
    #reservation_proposal_failed_payload{
        proposalId = ProposalId,
        errorMessage = ErrorMessage
    }) ->
    [{<<"proposalId">>, ProposalId}, {<<"errorMessage">>, ErrorMessage}];
payload_to_proplist(
    #board_update_succeeded_payload{
        proposalId = ProposalId,
        updateId = UpdateId,
        update = Update
    }) ->
    [
        {<<"proposalId">>, ProposalId},
        {<<"updateId">>, UpdateId},
        {<<"update">>, update_payload_to_proplist(Update)}
    ];
payload_to_proplist(
    #board_update_failed_payload{
        proposalId = ProposalId,
        errorMessage = ErrorMessage
    }) ->
    [{<<"proposalId">>, ProposalId}, {<<"errorMessage">>, ErrorMessage}];

% server broadcast
payload_to_proplist(
    #board_updated_payload{
        updateId = ChangeId,
        userId = UserId,
        intermediate = Intermediate,
        update = Update
    }) ->
    [
        {<<"updateId">>, ChangeId},
        {<<"userId">>, UserId},
        {<<"intermediate">>, Intermediate},
        {<<"update">>, update_payload_to_proplist(Update)}
    ];
payload_to_proplist(#user_payload{userId = UserId}) -> [{<<"userId">>, UserId}];
payload_to_proplist(
    #reservation_cancelled_payload{reservationId = ReservationId}) ->
    [{<<"reservationId">>, ReservationId}];
payload_to_proplist(
    #reservation_expired_payload{reservationId = ReservationId}) ->
    [{<<"reservationId">>, ReservationId}];
payload_to_proplist(
    #canvas_object_reserved_payload{
        reservationId = ReservationId,
        canvasObjectId = CanvasObjectId
    }) ->
    [{<<"reservationId">>, ReservationId}, {<<"canvasObjectId">>, CanvasObjectId}].

update_payload_to_proplist(
    #update_payload{
        canvasObjectId = CanvasObjectId,
        canvasObjectType = CanvasObjectType,
        operationType = OperationType,
        operation = Operation
    }) ->
    [
        {<<"canvasObjectId">>, CanvasObjectId},
        {<<"canvasObjectType">>, CanvasObjectType},
        {<<"operationType">>, OperationType},
        {<<"operation">>, canvas_object_operation_to_proplist(Operation)}
    ].

canvas_object_operation_to_proplist(
    #canvas_object_operation{
        canvasObjectOperationType = CanvasObjectOperationType,
        canvasObjectOperationPayload = CanvasObjectOperationPayload
    }) ->
    [{<<"canvasObjectOperationType">>, CanvasObjectOperationType} | CanvasObjectOperationPayload].