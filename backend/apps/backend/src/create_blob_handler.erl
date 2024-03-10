-module(create_blob_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2, handle_create_blob/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {{<<"image">>, <<"png">>, []}, handle_create_blob}
    ], Req, State}.

handle_create_blob(Req, State) ->
    {true, Req, State}.