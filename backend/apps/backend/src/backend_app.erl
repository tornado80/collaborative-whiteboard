%%%-------------------------------------------------------------------
%% @doc backend public API
%% @end
%%%-------------------------------------------------------------------

-module(backend_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("handlers_state_records.hrl").

start(_StartType, _StartArgs) ->
    AppDataDirectory = case application:get_env(backend, app_data_directory) of
        {ok, Dir} -> Dir;
        undefined -> filename:basedir(user_data, "whiteboard-backend")
    end,
    ok = case file:make_dir(AppDataDirectory) of
        ok -> ok;
        {error, eexist} -> ok;
        {error, Reason} -> {error, Reason}
    end,
    ok = application:set_env(backend, app_data_directory, AppDataDirectory),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", redirect_handler, undefined},
            {"/new", redirect_handler, undefined},
            {"/api", backend_handler, undefined},
            {"/api/rest/boards", create_board_handler, undefined},
            {"/api/rest/boards/:boardId", get_board_handler, #board_handler_state{}},
            {"/api/rest/boards/:boardId/blobs", create_blob_handler, #blob_handler_state{}},
            {"/api/rest/boards/:boardId/blobs/:blobId", get_blob_handler, #blob_handler_state{}},
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
