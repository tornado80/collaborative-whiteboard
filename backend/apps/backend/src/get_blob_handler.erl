-module(get_blob_handler).

-export([init/2, allowed_methods/2, content_types_provided/2, handle_get_blob/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"image">>, <<"png">>, []}, handle_get_blob}
    ], Req, State}.

handle_get_blob(Req, State) ->
    {true, Req, State}.