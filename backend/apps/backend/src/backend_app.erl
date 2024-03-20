%%%-------------------------------------------------------------------
%% @doc backend public API
%% @end
%%%-------------------------------------------------------------------

-module(backend_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("handlers_state_records.hrl").

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", frontend_new_board_redirector, undefined},
            {"/api", backend_handler, undefined},
            {"/api/rest/boards", create_board_handler, undefined},
            {"/api/rest/boards/:boardId", get_board_handler, #get_board_handler_state{}},
            {"/api/rest/boards/:boardId/blobs", create_blob_handler, #blob_handler_state{}},
            {"/api/rest/boards/:boardId/blobs/:blobId", get_blob_handler, undefined},
            {"/api/ws/boards/:boardId", websocket_handler, #websocket_handler_state{}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    backend_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(my_http_listener).

%% internal functions
