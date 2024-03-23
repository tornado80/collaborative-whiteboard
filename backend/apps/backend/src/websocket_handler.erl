-module(websocket_handler).

-include("handlers_state_records.hrl").
-include("event_payloads_records.hrl").

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
            find_board_controller_service(Req, State#websocket_handler_state{
                boardId = BoardId
            })
    end.

find_board_controller_service(Req, State) ->
    case boards_manager_service:try_get_board_controller_service(State#websocket_handler_state.boardId) of
        notfound -> 
            cowboy_req:reply(404, Req),
            {ok, Req, State};
        {ok, BoardControllerPid} -> 
            {cowboy_websocket, Req, State#websocket_handler_state{boardControllerPid = BoardControllerPid}}
    end.

websocket_init(State) ->
    {ok, State}.

websocket_handle(_Frame = {text, Json}, State) ->
    case websocket_event_parser:json_to_event(Json) of
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

websocket_info({broadcast, Event}, State) ->
    Json = websocket_event_parser:event_to_json(Event),
    {[{text, Json}], State};
websocket_info(_ErlangMsg, State) ->
    {[{active, true}], State}.

terminate(Reason = remote, _PartialReq, State) -> % client closed the connection
    inform_board_controller_service_of_ws_termination(State, {userLeftPermanently, Reason});
terminate(Reason = {remote, _Code, _Payload}, _PartialReq, State) -> % client closed the connection
    inform_board_controller_service_of_ws_termination(State, {userLeftPermanently, Reason});
terminate(Reason = stop, _PartialReq, State) -> % server closed the connection
    inform_board_controller_service_of_ws_termination(State, {userLeftPermanently, Reason});
terminate(Reason = timeout, _PartialReq, State) -> % connection closed due to inactivity
    inform_board_controller_service_of_ws_termination(State, {userLeftPermanently, Reason});
terminate(Reason = {error, _Error}, _PartialReq, State) -> % socket error
    inform_board_controller_service_of_ws_termination(State, {userLeftTemporarily, Reason});
terminate(Reason = {crash, _Class, _Reason}, _PartialReq, State) -> % handler crash
    inform_board_controller_service_of_ws_termination(State, {userLeftPermanently, Reason});
terminate(normal, _PartialReq, _State) -> ok.

% Internals

inform_board_controller_service_of_ws_termination(State, Reason) ->
    board_controller_service:end_session(
        State#websocket_handler_state.boardControllerPid, 
        State#websocket_handler_state.sessionRef,
        Reason),
    ok.