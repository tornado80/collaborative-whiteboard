-module(websocket_event_handlers).

-include("records.hrl").

-export([
    handle_event/2
]).

handle_event(
        #event{eventType = 'begin', eventId = EventId, eventContent = #begin_event{
            lastEventId=LastEventId, 
            sessionToken = SessionToken, 
            sessionType = SessionType
        }}, 
        #websocket_handler_state{boardControllerPid = Pid} = State) ->
    case {SessionType, SessionToken, LastEventId} of
        {new, _, _} -> 
            {ok, SessionToken, UserId} = board_controller_service:new_session(Pid),

            WelcomeData = jsone:encode([{<<"eventId">>, EventId+1}, {<<"eventType">>, <<"welcomeUser">>},{<<"sessionToken">>, SessionToken}, {<<"userId">>, UserId}]),
            
            {[{ text, WelcomeData}], State#websocket_handler_state{sessionToken = SessionToken}};
            
        {continue, undefined, _} -> 
            {[{close, <<"session token is not provided">>}], State};
        {continue, _, undefined} -> 
            {[{close, <<"last event id is not provided">>}], State};
        {continue, SessionToken0, LastEventId} when is_integer(LastEventId), is_binary(SessionToken0) -> 
            {ok, SessionToken1, UserId} = board_controller_service:continue_session(Pid, SessionToken0, LastEventId),
            WelcomeData = jsone:encode([{<<"eventId">>, EventId+1}, {<<"eventType">>, <<"welcomeUser">>},{<<"sessionToken">>, SessionToken1}, {<<"userId">>, UserId}]),

            {[{text, WelcomeData}], State#websocket_handler_state{sessionToken = SessionToken1}};
        {continue, _, _} ->
            {[{close, <<"malformed begin event">>}], State};
        {undefined, _, _} -> 
            {[{close, <<"invalid session type">>}], State}
    end;

handle_event(
        #event{eventType = 'reservationProposed', eventId = EventId, eventContent = #reservation_propose_event{
           canvasObjectId = CanvasObjectId,
           proposalId = ProposalId
        }}, 
        #websocket_handler_state{boardControllerPid = Pid, sessionToken = SessionToken, boardId = BoardId} = State) ->
       case board_controller_service:reserve_object(Pid, CanvasObjectId, ProposalId, SessionToken, BoardId ) of
        {ok, ReservationId, ExpirationTimeStamp} ->
            % reservation succcess 
            SuccessData = append_event_to_response_body(EventId+1,<<"reservationProposalSucceeded">>,[{<<"proposalId">>, ProposalId }, {<<"reservationId">>, ReservationId }, {<<"expirationTimestamp">>, ExpirationTimeStamp }]),
            {[{text, SuccessData}], State};

        already_reserved ->
            % already reserved
            UnsuccessData = append_event_to_response_body(EventId+1,<<"reservationProposalFailed">>, [{<<"proposalId">>, ProposalId }, {<<"errorMessage">>, <<"This object is already reserved.">> }] ),
            {[{text, UnsuccessData}], State};
        object_not_found ->
            UnsuccessData = append_event_to_response_body(EventId+1,<<"reservationProposalFailed">>, [{<<"proposalId">>, ProposalId }, {<<"errorMessage">>, <<"This object is not found.">> }] ),
            {[{text, UnsuccessData}], State};

        session_not_found ->
            UnsuccessData = append_event_to_response_body(EventId+1,<<"reservationProposalFailed">>, [{<<"proposalId">>, ProposalId }, {<<"errorMessage">>, <<"This session does not exist.">> }] ),
            {[{text, UnsuccessData}], State};

        board_not_found ->
            UnsuccessData = append_event_to_response_body(EventId+1,<<"reservationProposalFailed">>, [{<<"proposalId">>, ProposalId }, {<<"errorMessage">>, <<"The board does not exist.">> }] ),
            {[{text, UnsuccessData}], State};

        _ -> 
            UnsuccessData = append_event_to_response_body(EventId+1,<<"reservationProposalFailed">>, [{<<"proposalId">>, ProposalId }, {<<"errorMessage">>, <<"Unexpected behaviour.">> }] ),
            {[{text, UnsuccessData}], State}
        end;

handle_event(
        #event{eventType = 'reservationCancellationRequested', eventId = EventId, eventContent = #reservation_cancel_event{
           reservationId = ReservationId
        }}, 
        #websocket_handler_state{boardControllerPid = Pid, sessionToken = SessionToken, boardId = BoardId} = State) ->
        
        case board_controller_service:reserve_cancel(Pid, ReservationId, SessionToken, BoardId ) of
            {ok, ReservationId} ->
                SuccessData = append_event_to_response_body(EventId+1, <<"reservationCancelled">>, [{<<"reservationId">>, ReservationId }]),
                {[{text, SuccessData}], State};

            reservation_expired ->
            % reservation expired
                UnsuccessData = append_event_to_response_body(EventId+1,<<"reservationExpired">>, [{<<"reservationId">>, ReservationId }, {<<"errorMessage">>, <<"This object reservation is expired. Hence, it is illegal action.">> }] ),
                {[{text, UnsuccessData}], State};
            object_not_found ->
                UnsuccessData = append_event_to_response_body(EventId+1,<<"reservationExpired">>, [{<<"reservationId">>, ReservationId }, {<<"errorMessage">>, <<"This object is not found.">> }] ),
                {[{text, UnsuccessData}], State};

            session_not_found ->
                UnsuccessData = append_event_to_response_body(EventId+1,<<"reservationExpired">>, [{<<"reservationId">>, ReservationId }, {<<"errorMessage">>, <<"This session does not exist.">> }] ),
                {[{text, UnsuccessData}], State};

            board_not_found ->
                UnsuccessData = append_event_to_response_body(EventId+1,<<"reservationExpired">>, [{<<"reservationId">>, ReservationId }, {<<"errorMessage">>, <<"The board does not exist.">> }] ),
                {[{text, UnsuccessData}], State};

            _ -> 
                UnsuccessData = append_event_to_response_body(EventId+1,<<"reservationExpired">>, [{<<"reservationId">>, ReservationId }, {<<"errorMessage">>, <<"Unexpected behaviour.">> }] ),
                {[{text, UnsuccessData}], State}

                end;

handle_event(
        #event{eventType = 'boardUpdateProposed', eventId = EventId, eventContent = #board_update{
            proposalId = ProposalId,
            update = Update,
            canvasObjectId = CanvasObjectId
        }}, 
        #websocket_handler_state{boardControllerPid = Pid, sessionToken = SessionToken, boardId = BoardId} = State) ->
        
        case board_controller_service:board_update(Pid, SessionToken, BoardId, ProposalId, Update) of
            {ok, ProposalId,CanvasObjectId} ->
                SuccessData = append_event_to_response_body(EventId+1, <<"boardUpdateSucceeded">>, [{<<"proposalId">>, ProposalId }, {<<"canvasObjectId">>, CanvasObjectId }]),
                {[{text, SuccessData}], State};

            reservation_expired ->
            % reservation expired
                UnsuccessData = append_event_to_response_body(EventId+1,<<"boardUpdateFailed">>, [{<<"proposalId">>, ProposalId }, {<<"errorMessage">>, <<"This object reservation is expired. Hence, it is illegal action.">> }] ),
                {[{text, UnsuccessData}], State};
            object_not_found ->
                UnsuccessData = append_event_to_response_body(EventId+1,<<"boardUpdateFailed">>, [{<<"proposalId">>, ProposalId }, {<<"errorMessage">>, <<"This object is not found.">> }] ),
                {[{text, UnsuccessData}], State};

            session_not_found ->
                UnsuccessData = append_event_to_response_body(EventId+1,<<"boardUpdateFailed">>, [{<<"proposalId">>, ProposalId }, {<<"errorMessage">>, <<"This session does not exist.">> }] ),
                {[{text, UnsuccessData}], State};

            board_not_found ->
                UnsuccessData = append_event_to_response_body(EventId+1,<<"boardUpdateFailed">>, [{<<"proposalId">>, ProposalId }, {<<"errorMessage">>, <<"The board does not exist.">> }] ),
                {[{text, UnsuccessData}], State};

            _ -> 
                UnsuccessData = append_event_to_response_body(EventId+1,<<"boardUpdateFailed">>, [{<<"proposalId">>, ProposalId }, {<<"errorMessage">>, <<"Unexpected behaviour.">> }] ),
                {[{text, UnsuccessData}], State}
        end;
handle_event(
        #event{eventType = 'undoRequested', eventId = EventId,
        eventContent = #user{
            id = UserId
        }}, 
        #websocket_handler_state{boardControllerPid = Pid, sessionToken = SessionToken, boardId = BoardId} = State) ->
        
        case board_controller_service:undo_action(Pid, SessionToken, BoardId, UserId) of
            {ok, UserId} ->
                SuccessData = append_event_to_response_body(EventId+1, <<"undoSucceeded">>, [{<<"userId">>, UserId }]),
                {[{text, SuccessData}], State};

            reservation_expired ->
            % reservation expired
                UnsuccessData = append_event_to_response_body(EventId+1,<<"undoFailed">>, [{<<"userId">>, UserId }, {<<"errorMessage">>, <<"This object reservation is expired. Hence, it is illegal action.">> }] ),
                {[{text, UnsuccessData}], State};
            object_not_found ->
                UnsuccessData = append_event_to_response_body(EventId+1,<<"undoFailed">>, [{<<"userId">>, UserId }, {<<"errorMessage">>, <<"This object is not found.">> }] ),
                {[{text, UnsuccessData}], State};

            session_not_found ->
                UnsuccessData = append_event_to_response_body(EventId+1,<<"undoFailed">>, [{<<"userId">>, UserId }, {<<"errorMessage">>, <<"This session does not exist.">> }] ),
                {[{text, UnsuccessData}], State};

            board_not_found ->
                UnsuccessData = append_event_to_response_body(EventId+1,<<"undoFailed">>, [{<<"userId">>, UserId }, {<<"errorMessage">>, <<"The board does not exist.">> }] ),
                {[{text, UnsuccessData}], State};

            _ -> 
                UnsuccessData = append_event_to_response_body(EventId+1,<<"undoFailed">>, [{<<"userId">>, UserId }, {<<"errorMessage">>, <<"Unexpected behaviour.">> }] ),
                {[{text, UnsuccessData}], State}
        end;
handle_event(
        #event{eventType = 'redoRequested', eventId = EventId,
        eventContent = #user{
            id = UserId
        }}, 
        #websocket_handler_state{boardControllerPid = Pid, sessionToken = SessionToken, boardId = BoardId} = State) ->
        
        case board_controller_service:redo_action(Pid, SessionToken, BoardId, UserId) of
            {ok, UserId} ->
                SuccessData = append_event_to_response_body(EventId+1, <<"redoSucceeded">>, [{<<"userId">>, UserId }]),
                {[{text, SuccessData}], State};

            reservation_expired ->
            % reservation expired
                UnsuccessData = append_event_to_response_body(EventId+1,<<"redoFailed">>, [{<<"userId">>, UserId }, {<<"errorMessage">>, <<"This object reservation is expired. Hence, it is illegal action.">> }] ),
                {[{text, UnsuccessData}], State};
            object_not_found ->
                UnsuccessData = append_event_to_response_body(EventId+1,<<"redoFailed">>, [{<<"userId">>, UserId }, {<<"errorMessage">>, <<"This object is not found.">> }] ),
                {[{text, UnsuccessData}], State};

            session_not_found ->
                UnsuccessData = append_event_to_response_body(EventId+1,<<"redoFailed">>, [{<<"userId">>, UserId }, {<<"errorMessage">>, <<"This session does not exist.">> }] ),
                {[{text, UnsuccessData}], State};

            board_not_found ->
                UnsuccessData = append_event_to_response_body(EventId+1,<<"redoFailed">>, [{<<"userId">>, UserId }, {<<"errorMessage">>, <<"The board does not exist.">> }] ),
                {[{text, UnsuccessData}], State};

            _ -> 
                UnsuccessData = append_event_to_response_body(EventId+1,<<"redoFailed">>, [{<<"userId">>, UserId }, {<<"errorMessage">>, <<"Unexpected behaviour.">> }] ),
                {[{text, UnsuccessData}], State}

        end.

append_event_to_response_body(EventId, EventType, Other) ->
    jsone:encode([{<<"eventId">>, EventId }, {<<"eventType">>, EventType } | Other]).

