-module(websocket_event_handlers).

-include("handlers_state_records.hrl").
-include("event_payloads_records.hrl").

-export([
    handle_event/2
]).

handle_event(
        #event{
            eventType = 'begin',
            eventPayload = #begin_payload{
                lastEventId = LastEventId,
                sessionToken = SessionToken,
                sessionType = SessionType
        }}, 
        State = #websocket_handler_state{
            boardControllerPid = Pid,
            nextEventId = NextEventId
        }) ->
    WelcomeEvent = #event{
        eventId = NextEventId,
        eventType = <<"welcomeUser">>},
    NewState = State#websocket_handler_state{nextEventId = NextEventId + 1},
    case {SessionType, SessionToken, LastEventId} of
        {new, _, _} -> 
            {ok, SessionToken, UserId} = board_controller_service:new_session(Pid),
            NewEventContent = #welcome_user_payload{userId = UserId, sessionToken = SessionToken},
            WelcomeData = websocket_event_parser:event_to_json(WelcomeEvent#event{eventPayload = NewEventContent}),
            {[{text, WelcomeData}], NewState#websocket_handler_state{sessionToken = SessionToken}};
        {continue, undefined, _} -> 
            {[{close, <<"session token is not provided">>}], NewState};
        {continue, _, undefined} -> 
            {[{close, <<"last event id is not provided">>}], NewState};
        {continue, SessionToken0, LastEventId} when is_integer(LastEventId), is_binary(SessionToken0) -> 
            {ok, SessionToken1, UserId} = board_controller_service:continue_session(Pid, SessionToken0),
            NewEventContent = #welcome_user_payload{userId = UserId, sessionToken = SessionToken1},
            WelcomeData = websocket_event_parser:event_to_json(WelcomeEvent#event{eventPayload = NewEventContent}),
            {[{text, WelcomeData}], NewState#websocket_handler_state{sessionToken = SessionToken1}};
        {continue, _, _} ->
            {[{close, <<"malformed begin event">>}], NewState};
        {undefined, _, _} -> 
            {[{close, <<"invalid session type">>}], NewState}
    end;
handle_event(
        #event{
            eventType = reservationProposed,
            eventPayload = #reservation_proposed_payload{
                canvasObjectId = CanvasObjectId,
                proposalId = ProposalId
        }}, 
        State = #websocket_handler_state{
            boardControllerPid = Pid,
            sessionToken = SessionToken,
            nextEventId = NextEventId
        }) ->
    BaseEvent = #event{eventId = NextEventId},
    NewState = State#websocket_handler_state{nextEventId = NextEventId + 1},
    case board_controller_service:reserve_canvas_object(Pid, CanvasObjectId, SessionToken) of
        {ok, ReservationId, ExpirationTimeStamp} ->
            SuccessData = websocket_event_parser:event_to_json(
                BaseEvent#event{
                    eventType = <<"reservationProposalSucceeded">>,
                    eventPayload = #reservation_proposal_succeeded_payload{
                        proposalId = ProposalId,
                        reservationId = ReservationId,
                        expirationTimestamp = ExpirationTimeStamp
                    }
                }
            ),
            {[{text, SuccessData}], NewState};
        {error, Error} ->
            FailureData = websocket_event_parser:event_to_json(BaseEvent#event{
                eventType = <<"reservationProposalFailed">>,
                eventPayload = #reservation_proposal_failed_payload{
                    proposalId = ProposalId,
                    errorMessage = Error
                }
            }),
            {[{text, FailureData}], NewState}
    end;
handle_event(
        #event{
            eventType = reservationCancellationRequested,
            eventPayload = #reservation_cancellation_requested_payload{
                reservationId = ReservationId
            }
        },
        State = #websocket_handler_state{
            boardControllerPid = Pid,
            sessionToken = SessionToken
        }) ->
    board_controller_service:cancel_reservation(Pid, ReservationId, SessionToken),
    {[{active, true}], State};
handle_event(
        #event{
            eventType = boardUpdateProposed,
            eventPayload = #board_update_proposed_payload{
                proposalId = ProposalId,
                update = Update
            }
        },
        State = #websocket_handler_state{
            boardControllerPid = Pid,
            sessionToken = SessionToken,
            nextEventId = NextEventId}) ->
    NewState = State#websocket_handler_state{nextEventId = NextEventId + 1},
    case board_controller_service:update_board(Pid, Update, SessionToken) of
        ok ->
            SuccessData = websocket_event_parser:event_to_json(
                #event{
                    eventId = NextEventId,
                    eventType = <<"boardUpdateSucceeded">>,
                    eventPayload = #board_update_succeeded_payload{
                        proposalId = ProposalId
                    }
                }
            ),
            {[{text, SuccessData}], NewState};
        {error, Error} ->
            FailureData = websocket_event_parser:event_to_json(
                #event{
                    eventId = NextEventId,
                    eventType = <<"boardUpdateFailed">>,
                    eventPayload = #board_update_failed_payload{
                        proposalId = ProposalId,
                        errorMessage = Error
                    }
                }
            ),
            {[{text, FailureData}], NewState}
    end;
handle_event(
        #event{
            eventType = undoRequested
        },
        State = #websocket_handler_state{
            boardControllerPid = Pid,
            sessionToken = SessionToken
        }) ->
    board_controller_service:undo(Pid, SessionToken),
    {[{active, true}], State};
handle_event(
        #event{
            eventType = redoRequested
        },
        State = #websocket_handler_state{
            boardControllerPid = Pid,
            sessionToken = SessionToken
        }) ->
    board_controller_service:redo(Pid, SessionToken),
    {[{active, true}], State};
handle_event(Event, State) ->
    {[{close, io_lib:format(<<"unexpected event: ~p">>, [Event])}], State}.