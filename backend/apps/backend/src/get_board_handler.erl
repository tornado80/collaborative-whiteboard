-module(get_board_handler).

-include("records.hrl").

-export([
    init/2, 
    allowed_methods/2, 
    content_types_provided/2, 
    handle_get_board/2, 
    resource_exists/2, 
    malformed_request/2
]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, [{<<"charset">>, <<"utf-8">>}]}, handle_get_board}
    ], Req, State}.

malformed_request(Req, State) ->
    is_request_malformed('is board id provided?', Req, State).

is_request_malformed('is board id provided?', Req, State) ->
    BoardId = cowboy_req:binding(boardId, Req),
    case BoardId of
        undefined -> {true, Req, State};
        _ -> is_request_malformed('is board id valid uuid?', Req, State#get_board_handler_state{boardId = BoardId})
    end;
is_request_malformed('is board id valid uuid?', Req, State = #get_board_handler_state{boardId = BoardId}) ->
    case utility:is_valid_uuid(BoardId) of
        false -> {true, Req, State};
        true -> {false, Req, State}
    end.

resource_exists(Req, State = #get_board_handler_state{boardId = BoardId}) ->
    case boards_manager_service:try_get_board_controller_service(BoardId) of
        notfound -> {false, Req, State};
        {ok, Pid} -> {true, Req, State#get_board_handler_state{boardManagerPid = Pid}}
    end.

handle_get_board(Req, State) ->
    BoardState = board_controller_service:get_board_state(State#get_board_handler_state.boardManagerPid),
    {jsone:encode(BoardState), Req, State}.