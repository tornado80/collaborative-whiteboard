-module(create_board_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2, handle_create_board/2, content_types_provided/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_create_board}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, undefined}
    ], Req, State}.

handle_create_board(Req0, State) ->
    {ok, BoardId} = boards_manager_service:create_board(),
    Body = jsone:encode([{<<"boardId">>, BoardId}]),
    Req1 = cowboy_req:set_resp_body(Body, Req0),
    {{created, <<"/boards/", BoardId/binary>>}, Req1, State}.