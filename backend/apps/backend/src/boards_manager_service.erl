-module(boards_manager_service).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions
-export([
    create_board/0, 
    try_get_board_controller_service/1,
    try_get_board_cache_service/1
]).

%% State record
-record(state, {}).

-define(SERVER, ?MODULE).

%% API functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec create_board() -> {ok, BoardId :: binary()} | {error, Reason :: string()}.
create_board() ->
    gen_server:call(?SERVER, create_board).

-spec try_get_board_controller_service(binary()) -> {ok, pid()} | notfound.
try_get_board_controller_service(_BoardId) ->
    gen_server:call(?SERVER, {try_get_board_controller_service, _BoardId}).

-spec try_get_board_cache_service(binary()) -> {ok, pid()} | notfound.
try_get_board_cache_service(_BoardId) ->
    gen_server:call(?SERVER, {try_get_board_cache_service, _BoardId}).

%% Callback functions
init([]) ->
    {ok, boards_table} = dets:open_file(boards_table, [{type, set}, {file, "boards_table"}]),
    boards_table = ets:new(boards_table, [named_table, set]),
    boards_table = dets:to_ets(boards_table, boards_table),
    board_monitors = ets:new(board_monitors, [named_table, set]),
    {ok, #state{}}.

handle_call(create_board, _From, State) ->
    BoardId = utility:new_uuid(),
    create_new_board_supervisor(BoardId),
    ok = dets:insert(boards_table, {BoardId}),
    {reply, {ok, BoardId}, State};
handle_call({try_get_board_controller_service, BoardId}, _From, State) ->
    try_get_board_service(board_controller_service, BoardId, State);
handle_call({try_get_board_cache_service, BoardId}, _From, State) ->
    try_get_board_service(board_cache_service, BoardId, State);
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    % TODO: lookup ref in board_monitors and remove the entry
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    dets:close(boards_table),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internals

try_get_board_service(Service, BoardId, State) ->
    case ets:lookup(boards_table, BoardId) of
        [] ->
            {reply, notfound, State};
        [{BoardId}] -> % database like entry that has not been overwritten with Pid values so no board supervisor exists
            {BoardId, Map} = create_new_board_supervisor(BoardId),
            {Pid, _MonitorRef} = maps:get(Service, Map),
            {reply, {ok, Pid}, State};
        [{BoardId, Map}] ->
            {Pid, _MonitorRef} = maps:get(Service, Map),
            {reply, {ok, Pid}, State}
    end.

create_new_board_supervisor(BoardId) ->
    {ok, Supervisor} = supervisor:start_child(backend_sup,
        #{id => BoardId, start => {board_sup, start_link, [BoardId]}}
    ),
    true = ets:insert(boards_table, Entry = {BoardId, monitor_supervisor_and_children(BoardId, Supervisor)}),
    Entry.

monitor_supervisor_and_children(BoardId, Supervisor) ->
    Children = supervisor:which_children(Supervisor),
    MonitorRef = erlang:monitor(process, Supervisor),
    true = ets:insert(board_monitors, {MonitorRef, board_sup, BoardId}),
    {_, _, Map} = lists:foldl(
        fun compare_and_monitor_child/2,
        {
            BoardId,
            [
                board_database_service,
                board_cache_service,
                board_controller_service
            ],
            #{board_sup => {Supervisor, MonitorRef}}
        },
        Children
    ),
    Map.

compare_and_monitor_child({_, restarting, _, _}, Acc) ->
    Acc;
compare_and_monitor_child({_, undefined, _, _}, Acc) ->
    Acc;
compare_and_monitor_child({Service, Pid, _Type, _Modules}, Acc = {BoardId, ExpectedChildren, Map}) ->
    case lists:member(Service, ExpectedChildren) of
        true ->
            MonitorRef = erlang:monitor(process, Pid),
            ets:insert(board_monitors, {MonitorRef, Service, BoardId}),
            {BoardId, ExpectedChildren, Map#{Service => {Pid, MonitorRef}}};
        false ->
            Acc
    end.