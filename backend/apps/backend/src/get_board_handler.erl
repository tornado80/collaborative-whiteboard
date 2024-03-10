-module(get_board_handler).

-export([init/2, allowed_methods/2, content_types_provided/2, handle_get_board/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, [{<<"charset">>, <<"utf-8">>}]}, handle_get_board}
    ], Req, State}.

handle_get_board(Req, State) ->
    {true, Req, State}.