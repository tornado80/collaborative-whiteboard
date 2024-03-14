-module(boards_manager_service).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions
-export([
    create_board/0, 
    try_get_board_controller_service/1,
    try_get_board_cache_service/1, 
    is_session_token_valid/1
]).

%% State record
-record(state, {}).

-define(SERVER, ?MODULE).

%% API functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec create_board() -> {ok, BoardId :: binary()} | {error, Reason :: string()}.
create_board() ->
    % TODO: perhaps gen_server call to the service
    {ok, utility:new_uuid()}.

-spec try_get_board_controller_service(binary()) -> {ok, pid()} | notfound.
try_get_board_controller_service(BoardId) ->
    % TODO: if board id exists in the database but there is no controller service, spawn one
    % otherwise, if board id exists in the database and there is a controller service, return it
    % otherwise, return notfound
    notfound.

-spec try_get_board_cache_service(binary()) -> {ok, pid()} | notfound.
try_get_board_cache_service(SessionToken) -> 
    notfound.

-spec is_session_token_valid(binary()) -> boolean().
is_session_token_valid(SessionToken) ->
    false.

%% Callback functions
init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.