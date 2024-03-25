-module(websocket_event_handlers).

-include("handlers_state_records.hrl").
-include("event_payloads_records.hrl").
-include("board_state_records.hrl").

-export([
    handle_event/2
]).

handle_event(
        #event{
            eventType = 'begin',
            eventPayload = #begin_payload{
                sessionToken = SessionToken,
                sessionType = SessionType
        }}, 
        State = #websocket_handler_state{
            boardControllerPid = Pid
        }) ->
    WelcomeEvent = #event{
        eventType = <<"welcomeUser">>},
    case {SessionType, SessionToken} of
        {new, _} ->
            {ok, Session, SessionRef} = board_controller_service:new_session(Pid),
            NewEventContent = #welcome_user_payload{
                userId = Session#session.userId,
                sessionToken = Session#session.sessionToken,
                userName = Session#session.userName,
                color = Session#session.color},
            WelcomeData = websocket_event_parser:event_to_json(WelcomeEvent#event{eventPayload = NewEventContent}),
            {[{text, WelcomeData}], State#websocket_handler_state{
                sessionToken = SessionToken, sessionRef = SessionRef}};
        {continue, undefined} ->
            {[{close, <<"session token is not provided">>}], State};
        {continue, SessionToken} when is_binary(SessionToken) ->
            {ok, Session, SessionRef} =
                board_controller_service:continue_session(Pid, SessionToken),
            NewEventContent = #welcome_user_payload{
                userId = Session#session.userId,
                sessionToken = Session#session.sessionToken,
                userName = Session#session.userName,
                color = Session#session.color},
            WelcomeData = websocket_event_parser:event_to_json(WelcomeEvent#event{eventPayload = NewEventContent}),
            {[{text, WelcomeData}], State#websocket_handler_state{
                sessionToken = Session#session.sessionToken, sessionRef = SessionRef}};
        {continue, _} ->
            {[{close, <<"malformed begin event">>}], State};
        {undefined, _} ->
            {[{close, <<"invalid session type">>}], State}
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
            sessionRef = SessionRef
        }) ->
    case board_controller_service:reserve_canvas_object(Pid, CanvasObjectId, SessionRef) of
        {ok, ReservationId, ExpirationTimeStamp} ->
            SuccessData = websocket_event_parser:event_to_json(
                #event{
                    eventType = <<"reservationProposalSucceeded">>,
                    eventPayload = #reservation_proposal_succeeded_payload{
                        proposalId = ProposalId,
                        reservationId = ReservationId,
                        expirationTimestamp = ExpirationTimeStamp
                    }
                }
            ),
            {[{text, SuccessData}], State};
        {error, Error} ->
            FailureData = websocket_event_parser:event_to_json(#event{
                eventType = <<"reservationProposalFailed">>,
                eventPayload = #reservation_proposal_failed_payload{
                    proposalId = ProposalId,
                    errorMessage = Error
                }
            }),
            {[{text, FailureData}], State}
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
            sessionRef = SessionRef
        }) ->
    board_controller_service:cancel_reservation(Pid, ReservationId, SessionRef),
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
            sessionRef = SessionRef}) ->
    case board_controller_service:update_board(Pid, Update, SessionRef) of
        {ok, UpdatedId, UpdatePayload} ->
            SuccessData = websocket_event_parser:event_to_json(
                #event{
                    eventType = <<"boardUpdateSucceeded">>,
                    eventPayload = #board_update_succeeded_payload{
                        proposalId = ProposalId
                    }
                }
            ),
            {[{text, SuccessData}], State};
        {error, Error} ->
            FailureData = websocket_event_parser:event_to_json(
                #event{
                    eventType = <<"boardUpdateFailed">>,
                    eventPayload = #board_update_failed_payload{
                        proposalId = ProposalId,
                        errorMessage = Error
                    }
                }
            ),
            {[{text, FailureData}], State}
    end;
handle_event(
        #event{
            eventType = undoRequested
        },
        State = #websocket_handler_state{
            boardControllerPid = Pid,
            sessionRef = SessionRef
        }) ->
    board_controller_service:undo(Pid, SessionRef),
    {[{active, true}], State};
handle_event(
        #event{
            eventType = redoRequested
        },
        State = #websocket_handler_state{
            boardControllerPid = Pid,
            sessionRef = SessionRef
        }) ->
    board_controller_service:redo(Pid, SessionRef),
    {[{active, true}], State};
handle_event(Event, State) ->
    {[{close, io_lib:format(<<"unexpected event: ~p">>, [Event])}], State}.