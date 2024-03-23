-module(board_cache_service).
-behaviour(gen_server).

-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions
-export([create_blob/2, try_get_blob/2]).

%% State record
-record(state, {
    boardId :: binary(),
    blobs_table :: ets:tid(),
    supervisorPid :: pid()
}).

%% API functions
start_link(BoardId, SupervisorPid) ->
    gen_server:start_link(?MODULE, {BoardId, SupervisorPid}, []).

-spec create_blob(pid(), binary()) -> {ok, binary()}.
create_blob(Pid, Body) ->
    gen_server:call(Pid, {create_blob, Body}).

-spec try_get_blob(pid(), binary()) -> {ok, binary()} | notfound.
try_get_blob(Pid, BlobId) ->
    gen_server:call(Pid, {try_get_blob, BlobId}).

%% Callback functions
init({BoardId, SupervisorPid}) ->
    process_flag(trap_exit, true),
    boards_manager_service:request_to_be_registered_and_monitored(BoardId, ?MODULE, self()),
    BlobsTable = ets:new(board_blobs_table, [set]),
    spawn(fun() -> gen_server:cast(self(), load_blobs_from_database) end),
    lager:info("Started board cache service for board ~p at ~p", [BoardId, self()]),
    {ok, #state{
        boardId = BoardId,
        blobs_table = BlobsTable,
        supervisorPid = SupervisorPid
    }}.

handle_call({create_blob, Body}, _From, State) ->
    BlobId = utility:new_uuid(),
    true = ets:insert(State#state.blobs_table, {BlobId, Body}),
    insert_blob_into_database(State#state.supervisorPid, BlobId, Body),
    {reply, {ok, BlobId}, State};
handle_call({try_get_blob, BlobId}, _From, State) ->
    case ets:lookup(State#state.blobs_table, BlobId) of
        [] -> {reply, notfound, State};
        [{_, Body}] -> {reply, {ok, Body}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(load_blobs_from_database, State) ->
    load_blobs_from_database(State#state.supervisorPid, State#state.blobs_table),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

load_blobs_from_database(SupervisorPid, BlobsTable) ->
    DatabaseServicePid = board_sup:get_board_service_pid(SupervisorPid, board_database_service),
    Blobs = board_database_service:get_blobs_table(DatabaseServicePid),
    true = ets:insert(BlobsTable, Blobs).

insert_blob_into_database(SupervisorPid, BlobId, Body) ->
    DatabaseServicePid = board_sup:get_board_service_pid(SupervisorPid, board_database_service),
    board_database_service:insert_blob(DatabaseServicePid, BlobId, Body).