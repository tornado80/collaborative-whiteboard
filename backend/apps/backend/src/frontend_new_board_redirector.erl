-module(frontend_new_board_redirector).

-export([init/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, <<"*">>}, <<"*">>}], Req, State}.