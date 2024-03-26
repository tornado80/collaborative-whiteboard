-module(create_blob_handler).

-include("handlers_state_records.hrl").

-export([init/2, allowed_methods/2, content_types_accepted/2, handle_create_blob/2, content_types_provided/2, malformed_request/2, resource_exists/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {{<<"image">>, <<"png">>, []}, handle_create_blob},
        {{<<"image">>, <<"jpg">>, []}, handle_create_blob},
        {{<<"image">>, <<"jpeg">>, []}, handle_create_blob}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, undefined}
    ], Req, State}.

% malformed_request(Req, State) ->
%     SessionToken = cowboy_req:header(<<"session-token">>, Req),
%     case SessionToken of
%         undefined -> {true, Req, State};
%         _ -> {false, Req, State#blob_handler_state{sessionToken = SessionToken}}
%     end.

malformed_request(Req, State) ->
    is_request_malformed('is board id provided?', Req, State).

is_request_malformed('is board id provided?', Req, State) ->
    BoardId = cowboy_req:binding(boardId, Req),
    case BoardId of
        undefined -> {true, Req, State};
        _ -> is_request_malformed('is board id valid uuid?', Req, State#blob_handler_state{boardId = BoardId})
    end;
is_request_malformed('is board id valid uuid?', Req, State = #blob_handler_state{boardId = BoardId}) ->
    case utility:is_valid_uuid(BoardId) of
        false -> {true, Req, State};
        true -> {false, Req, State}
    end.

% is_authorized(Req, State) ->
%     case boards_manager_service:try_get_board_cache_service(State#blob_handler_state.sessionToken) of
%         notfound -> {false, Req, State};
%         {ok, BoardCacheServicePid} -> {true, Req, State#blob_handler_state{boardCacheServicePid = BoardCacheServicePid}}
%     end.

resource_exists(Req, State = #blob_handler_state{boardId = BoardId}) ->
    case boards_manager_service:try_get_board_cache_service(BoardId) of
        notfound -> {false, Req, State};
        service_not_available ->
            Req1 = cowboy_req:reply(503, #{<<"retry-after">> => 10},
                <<"Board cache service is not available now. Retry in a few seconds.">>, Req),
            {stop, Req1, State};
        {ok, Pid} -> {true, Req, State#blob_handler_state{boardCacheServicePid = Pid}}
    end.

handle_create_blob(Req0, State) ->
    {ok, Body, Req1} = read_body(Req0, <<>>),
    {ok, BlobId} = board_cache_service:create_blob(State#blob_handler_state.boardCacheServicePid, Body),
    NewBody = jsone:encode([{<<"blobId">>, BlobId}]),
    Req2 = cowboy_req:set_resp_body(NewBody, Req1),
    {{created, BlobId}, Req2, State}.

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.