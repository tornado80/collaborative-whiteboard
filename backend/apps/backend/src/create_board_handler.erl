-module(create_board_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2, handle_create_board/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, [{<<"charset">>, <<"utf-8">>}]}, handle_create_board}
    ], Req, State}.

handle_create_board(Req, State) ->
    {true, Req, State}.