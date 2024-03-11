-module(websocket_handler).

-export([
    init/2, 
    websocket_init/1, 
    websocket_handle/2, 
    websocket_info/2
]).

% Callbacks

init(Req, State) ->
    NewState = create_new_state_with_previous_session_token_if_provided(Req, State),
    {cowboy_websocket, Req, NewState}.

websocket_init(State) ->
    lager:info("Websocket connection established"),
    {ok, State}.

websocket_handle(Frame = {text, _}, State) ->
    lager:info("Received text frame"),
    {[{text, io_lib:format("Received text frame: ~p~n", [Frame])}], State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_ErlangMsg, State) ->
    lager:info("Received Erlang message"),
    {ok, State}.

% Internals

create_new_state_with_previous_session_token_if_provided(Req, State) ->
    case cowboy_req:header(<<"session-token">>, Req) of
        undefined -> State;
        SessionToken -> State#{session_token => SessionToken}
    end.