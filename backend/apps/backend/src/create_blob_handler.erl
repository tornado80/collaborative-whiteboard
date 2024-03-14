-module(create_blob_handler).

-include("records.hrl").

-export([init/2, allowed_methods/2, content_types_accepted/2, handle_create_blob/2, content_types_provided/2, malformed_request/2, is_authorized/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {{<<"image">>, <<"png">>, []}, handle_create_blob}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, [{<<"charset">>, <<"utf-8">>}]}, undefined}
    ], Req, State}.

malformed_request(Req, State) ->
    SessionToken = cowboy_req:header(<<"session-token">>, Req),
    case SessionToken of
        undefined -> {true, Req, State};
        _ -> {false, Req, State#blob_handler_state{sessionToken = SessionToken}}
    end.

is_authorized(Req, State) ->
    case boards_manager_service:try_get_board_cache_service(State#blob_handler_state.sessionToken) of
        notfound -> {false, Req, State};
        {ok, BoardCacheServicePid} -> {true, Req, State#blob_handler_state{boardCacheServicePid = BoardCacheServicePid}}
    end.

handle_create_blob(Req0, State) ->
    {ok, Body} = read_body(Req0, <<>>),
    {ok, BlobId} = board_cache_service:create_blob(State#blob_handler_state.boardCacheServicePid, Body),
    Body = jsone:encode([{<<"blobId">>, BlobId}]),
    Req1 = cowboy_req:set_resp_body(Body, Req0),
    {{created, BlobId}, Req1, State}.

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.