-module(board_database_service).
-behaviour(gen_server).

-include("board_state_records.hrl").

-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions
-export([get_objects_table/1, get_blobs_table/1, insert_blob/3,
    insert_object/4, delete_object/2]).

%% State record
-record(state, {
    board_id :: binary(),
    board_objects_table :: dets:tid(),
    board_blobs_table :: dets:tid()
}).

%% API functions
start_link(BoardId, SupervisorPid) ->
    gen_server:start_link(?MODULE, {BoardId, SupervisorPid}, []).

get_objects_table(Pid) ->
    gen_server:call(Pid, get_objects_table).

get_blobs_table(Pid) ->
    gen_server:call(Pid, get_blobs_table).

insert_blob(Pid, BlobId, Body) ->
    gen_server:cast(Pid, {insert_blob, BlobId, Body}).

insert_object(Pid, ObjectId, ObjectType, Object) ->
    gen_server:cast(Pid, {insert_object, ObjectId, ObjectType, Object}).

delete_object(Pid, ObjectId) ->
    gen_server:cast(Pid, {delete_object, ObjectId}).

%% Callback functions
init({BoardId, _SupervisorPid}) ->
    process_flag(trap_exit, true),
    boards_manager_service:request_to_be_registered_and_monitored(BoardId, ?MODULE, self()),
    {ok, AppDataDirectory} = application:get_env(backend, app_data_directory),
    BoardDirectory = filename:join(AppDataDirectory, BoardId),
    ok = case file:make_dir(BoardDirectory) of
        ok -> ok;
        {error, eexist} -> ok;
        {error, Reason} -> {error, Reason}
    end,
    BoardObjectsTablePath = binary_to_list(filename:join([BoardDirectory, <<"objects_table_", BoardId/binary>>])),
    BoardBlobsTablePath = binary_to_list(filename:join([BoardDirectory, <<"blobs_table_", BoardId/binary>>])),
    lager:info("Paths for board ~p: ~p, ~p", [BoardId, BoardObjectsTablePath, BoardBlobsTablePath]),
    BoardObjectsTable = dets_open_file({objects, BoardId}, [{file, BoardObjectsTablePath}]),
    BoardBlobsTable = dets_open_file({blobs, BoardId}, [{file, BoardBlobsTablePath}]),
    lager:info("Started database service for board ~p at ~p", [BoardId, self()]),
    {ok, #state{
        board_id = BoardId,
        board_objects_table = BoardObjectsTable,
        board_blobs_table = BoardBlobsTable
    }}.

handle_call(get_objects_table, _From, State) ->
    ObjectsTable = dets_to_list(State#state.board_objects_table),
    lager:info("Objects table: ~p", [ObjectsTable]),
    {reply, ObjectsTable, State};
handle_call(get_blobs_table, _From, State) ->
    BlobsTable = dets_to_list(State#state.board_blobs_table),
    {reply, BlobsTable, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({delete_object, ObjectId}, State) ->
    ok = dets:delete(State#state.board_objects_table, ObjectId),
    {noreply, State};
handle_cast({insert_blob, BlobId, Body}, State) ->
    ok = dets:insert(State#state.board_blobs_table, {BlobId, Body}),
    {noreply, State};
handle_cast({insert_object, ObjectId, ObjectType, Object}, State) ->
    ok = dets:insert(State#state.board_objects_table, {ObjectId, ObjectType, Object}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(shutdown, State) -> % shutdown by supervisor
    % get list of blobs used in objects
    Blobs = dets:foldl(
        fun({_, image, #image{blobId = BlobId}}, Acc) ->
            [BlobId | Acc];
        (_, Acc) ->
            Acc
        end,
        [], State#state.board_objects_table),
    % delete blobs not in the list
    dets:foldl(
        fun({BlobId, _}, _) ->
            case lists:member(BlobId, Blobs) of
                true -> ok;
                false -> dets:delete(State#state.board_blobs_table, BlobId)
            end
        end,
        Blobs, State#state.board_blobs_table),
    % close tables
    dets:close(State#state.board_objects_table),
    dets:close(State#state.board_blobs_table),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

dets_to_list(Table) ->
    dets:foldl(fun(Object, Acc) -> [Object | Acc] end, [], Table).

dets_open_file(Name, Args) ->
    case dets:open_file(Name, Args) of
        {ok, Table} -> Table;
        {error, Reason} -> lager:error("Failed to open table ~p: ~p", [Name, Reason]), exit(error)
    end.