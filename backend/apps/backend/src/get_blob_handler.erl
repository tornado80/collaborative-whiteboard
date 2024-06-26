-module(get_blob_handler).

-include("handlers_state_records.hrl").

-export([init/2, allowed_methods/2, content_types_provided/2, handle_get_blob/2, malformed_request/2, resource_exists/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"image">>, <<"png">>, '*'}, handle_get_blob}
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
        true -> is_request_malformed('is blob id provided?', Req, State)
    end;
is_request_malformed('is blob id provided?', Req, State) ->
    BlobId = cowboy_req:binding(blobId, Req),
    case blobId of 
        undefined -> {true, Req, State };
        _ -> is_request_malformed('is blob id valid uuid?', Req, State#blob_handler_state{blobId = BlobId})
    end;

is_request_malformed('is blob id valid uuid?', Req, State = #blob_handler_state{blobId = BlobId}) ->
    case utility:is_valid_uuid(BlobId) of
        false -> {true, Req, State};
        true -> {false, Req, State}
    end.


% is_authorized(Req, State) ->
%     case boards_manager_service:try_get_board_cache_service(State#blob_handler_state.sessionToken) of
%         notfound -> {false, Req, State};
%         {ok, BoardCacheServicePid} -> {true, Req, State#blob_handler_state{boardCacheServicePid = BoardCacheServicePid}}
%     end.

resource_exists(Req, State = #blob_handler_state{boardId = BoardId, blobId = BlobId}) ->
    lager:info("Checking if blob ~p exists at board ~p cache service", [BlobId, BoardId]),
    case boards_manager_service:try_get_board_cache_service(BoardId) of
        notfound -> {false, Req, State};
        service_not_available ->
            lager:info("Board ~p cache service is not available now. Retry in a few seconds.", [BoardId]),
            Req1 = cowboy_req:reply(503, #{<<"retry-after">> => 10},
                <<"Board cache service is not available now. Retry in a few seconds.">>, Req),
            {stop, Req1, State};
        {ok, Pid} ->
            lager:info("Trying get blob ~p from board ~p cache service", [BlobId, BoardId]),
            case board_cache_service:try_get_blob(Pid, BlobId) of
                notfound ->
                    lager:info("Blob ~p not found at board ~p cache service", [BlobId, BoardId]),
                    {false, Req, State};
                {ok, Blob} ->
                    lager:info("Blob ~p received from board ~p cache service", [BlobId, BoardId]),
                    {true, Req, State#blob_handler_state{blob = Blob}}
            end
    end.

handle_get_blob(Req, State) ->
    {State#blob_handler_state.blob, Req, State}.