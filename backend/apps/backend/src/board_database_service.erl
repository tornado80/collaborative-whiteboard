-module(board_database_service).
-behaviour(gen_server).

-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions
-export([get_all_tables/1]).

%% State record
-record(state, {
    board_id :: binary(),
    board_objects_table :: dets:tid(),
    board_sessions_table :: dets:tid(),
    board_session_tokens_table :: dets:tid()
}).

%% API functions
start_link(BoardId, SupervisorPid) ->
    gen_server:start_link(?MODULE, {BoardId, SupervisorPid}, []).

get_all_tables(Pid) ->
    gen_server:call(Pid, get_all_tables).

%% Callback functions
init({BoardId, _SupervisorPid}) ->
    %process_flag(trap_exit, true),
    boards_manager_service:request_to_be_registered_and_monitored(BoardId, ?MODULE, self()),
    {ok, BoardObjectsTable} = dets:open_file(binary_to_list(<<BoardId/binary, "_objects_table">>), [{type, set}]),
    %{ok, BoardSessionsTable} = dets:open_file(binary_to_list(<<BoardId, "_sessions_table">>), [{type, set}]),
    %{ok, BoardSessionTokensTable} = dets:open_file(binary_to_list(<<BoardId, "_session_tokens_table">>), [{type, set}]),
    lager:info("Started database service for board ~p", [BoardId]),
    {ok, #state{
        board_id = BoardId,
        board_objects_table = BoardObjectsTable
        %board_sessions_table = BoardSessionsTable,
        %board_session_tokens_table = BoardSessionTokensTable
    }}.

handle_call(get_all_tables, _From, State) ->
    ObjectsTable = dets_to_list(State#state.board_objects_table),
    %SessionsTable = dets_to_list(State#state.board_sessions_table),
    %TokensTable = dets_to_list(State#state.board_session_tokens_table),
    {reply, ObjectsTable, State}; % {ObjectsTable, SessionsTable, TokensTable}
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

dets_to_list(Table) ->
    dets:foldl(fun(Object, Acc) -> [Object | Acc] end, [], Table).