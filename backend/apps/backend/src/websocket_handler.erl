-module(websocket_handler).

-include("records.hrl").

-export([
    init/2, 
    websocket_init/1, 
    websocket_handle/2, 
    websocket_info/2,
    terminate/3
]).

% Callbacks

init(Req, State) ->
    case cowboy_req:binding(boardId, Req) of
        undefined -> 
            cowboy_req:reply(400, Req),
            {ok, Req, State};
        BoardId -> 
            find_board_controller_service(Req, State#websocket_handler_state{boardId = BoardId})
    end.

find_board_controller_service(Req, State) ->
    case boards_manager_service:try_get_board_controller_service(State#websocket_handler_state.boardId) of
        notfound -> 
            cowboy_req:reply(404, Req),
            {ok, Req, State};
        {ok, BoardManagerPid} -> 
            {cowboy_websocket, Req, State#websocket_handler_state{boardManagerPid = BoardManagerPid}}
    end.

websocket_init(State) ->
    {ok, State}.

websocket_handle(_Frame = {text, Data}, State) ->
    case websocket_event_parser:parse(Data) of
        {ok, Event} -> 
            websocket_event_handlers:handle_event(Event, State);
        {error, Reason} -> 
            lager:error(Reason),
            {[{close, Reason}], State}
    end;
websocket_handle(_Frame = {binary, _Data}, State) ->
    Reason = <<"this endpoint does not accept binary stream">>,
    lager:error(Reason),
    {[{close, Reason}], State};
websocket_handle(_Frame, State) ->
    {[{active, true}], State}.

websocket_info(_ErlangMsg, State) ->
    {[{active, true}], State}.

terminate(remote, _PartialReq, State) ->
    inform_board_controller_service_of_termination(State, userLeftPermanently);
terminate({remote, _, _}, _PartialReq, State) ->
    inform_board_controller_service_of_termination(State, userLeftPermanently);
terminate(stop, _PartialReq, State) ->
    inform_board_controller_service_of_termination(State, userLeftPermanently);
terminate(timeout, _PartialReq, State) ->
    inform_board_controller_service_of_termination(State, userLeftTemporarily);
terminate({error, _}, _PartialReq, State) ->
    inform_board_controller_service_of_termination(State, userLeftTemporarily);
terminate(normal, _PartialReq, State) ->
    inform_board_controller_service_of_termination(State, userLeftTemporarily).

% Internals

inform_board_controller_service_of_termination(State, Status) ->
    board_controller_service:unsubscribe_from_board(
        State#websocket_handler_state.boardManagerPid, 
        State#websocket_handler_state.sessionToken, 
        self(), 
        Status),
    ok.