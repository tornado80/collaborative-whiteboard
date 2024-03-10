%%%-------------------------------------------------------------------
%% @doc backend public API
%% @end
%%%-------------------------------------------------------------------

-module(backend_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/rest/boards", create_board_handler, []},
            {"/api/rest/boards/:boardId", get_board_handler, []},
            {"/api/rest/blobs", create_blob_handler, []},
            {"/api/rest/blobs/:blobId", get_blob_handler, []},
            {"/api/ws", websocket_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    backend_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
