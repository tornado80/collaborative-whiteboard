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
    case cowboy_req:binding(<<"board-id">>, Req) of
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

is_session_token_provided(Req, State) ->
    case cowboy_req:header(<<"session-token">>, Req) of
        undefined -> 
            {ok, SessionToken} = board_controller_service:create_session_token(State#websocket_handler_state.boardManagerPid),
            {cowboy_websocket, Req, State#websocket_handler_state{sessionToken = SessionToken}};
        SessionToken -> 
            is_session_token_valid(Req, State#websocket_handler_state{sessionToken = SessionToken})
    end.

is_session_token_valid(Req, State) ->
    case board_controller_service:is_session_token_valid(State#websocket_handler_state.sessionToken) of
        false -> 
            cowboy_req:reply(401, Req),
            {ok, Req, State};
        true -> 
            {cowboy_websocket, Req, State}
    end.

websocket_init(State = #websocket_handler_state{boardManagerPid = BoardManagerPid, sessionToken = SessionToken}) ->
    board_controller_service:subscribe_to_board(BoardManagerPid, SessionToken, self()),
    {ok, State}.

websocket_handle(Frame = {text, _}, State) ->
    {[Frame], State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_ErlangMsg, State) ->
    {ok, State}.

terminate(remote, PartialReq, State) ->
    inform_board_controller_service_of_termination(State, userLeftPermanently);
terminate({remote, _, _}, PartialReq, State) ->
    inform_board_controller_service_of_termination(State, userLeftPermanently);
terminate(stop, PartialReq, State) ->
    inform_board_controller_service_of_termination(State, userLeftPermanently);
terminate(timeout, PartialReq, State) ->
    inform_board_controller_service_of_termination(State, userLeftTemporarily);
terminate({error, _}, PartialReq, State) ->
    inform_board_controller_service_of_termination(State, userLeftTemporarily);
terminate(normal, PartialReq, State) ->
    inform_board_controller_service_of_termination(State, userLeftTemporarily).

% Internals

inform_board_controller_service_of_termination(State, Status) ->
    board_controller_service:unsubscribe_from_board(
        State#websocket_handler_state.boardManagerPid, 
        State#websocket_handler_state.sessionToken, 
        self(), 
        Status),
    ok.