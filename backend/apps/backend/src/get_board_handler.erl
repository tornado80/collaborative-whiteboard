-module(get_board_handler).

-include("handlers_state_records.hrl").

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
        {{<<"application">>, <<"json">>, '*'}, handle_get_board}
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
        service_not_available ->
            Req1 = cowboy_req:reply(503, #{<<"retry-after">> => 10},
                <<"Board controller service is not available now. Retry in a few seconds.">>, Req),
            {stop, Req1, State};
        {ok, Pid} -> {true, Req, State#get_board_handler_state{boardControllerPid = Pid}}
    end.

handle_get_board(Req, State) ->
    BoardStateJson = board_controller_service:get_board_state(State#get_board_handler_state.boardControllerPid),
    {BoardStateJson, Req, State}.