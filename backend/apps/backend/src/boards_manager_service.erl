-module(boards_manager_service).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions
-export([
    create_board/0, 
    try_get_board_controller_service/1,
    try_get_board_cache_service/1,
    try_get_board_service/2,
    request_to_be_registered_and_monitored/3
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

-spec try_get_board_controller_service(binary()) -> {ok, pid()} | notfound | service_not_available.
try_get_board_controller_service(BoardId) ->
    gen_server:call(?SERVER, {try_get_board_controller_service, BoardId}).

-spec try_get_board_cache_service(binary()) -> {ok, pid()} | notfound | service_not_available.
try_get_board_cache_service(BoardId) ->
    gen_server:call(?SERVER, {try_get_board_cache_service, BoardId}).

request_to_be_registered_and_monitored(BoardId, Service, Pid) ->
    gen_server:cast(?SERVER, {register_and_monitor, BoardId, Service, Pid}).

try_get_board_service(Service, BoardId) ->
    gen_server:call(?SERVER, {try_get_board_service, Service, BoardId}).

%% Callback functions
init([]) ->
    process_flag(trap_exit, true),
    %{ok, FileName} = application:get_env(backend, boards_table_db_name),
    {ok, boards_table} = dets:open_file(boards_table, [{type, set}, {file, "boards_table"}]),
    boards_table = ets:new(boards_table, [named_table, set]),
    boards_table = dets:to_ets(boards_table, boards_table),
    boards_monitors = ets:new(boards_monitors, [named_table, set]),
    {ok, #state{}}.

handle_call(create_board, _From, State) ->
    BoardId = utility:new_uuid(),
    create_new_board_supervisor(BoardId),
    ok = dets:insert(boards_table, {BoardId}),
    {reply, {ok, BoardId}, State};
handle_call({try_get_board_service, Service, BoardId}, _From, State) ->
    try_get_board_service(Service, BoardId, State);
handle_call({try_get_board_controller_service, BoardId}, _From, State) ->
    try_get_board_service(board_controller_service, BoardId, State);
handle_call({try_get_board_cache_service, BoardId}, _From, State) ->
    try_get_board_service(board_cache_service, BoardId, State);
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({register_and_monitor, BoardId, Service, Pid}, State) ->
    case ets:lookup(boards_table, BoardId) of
        [] ->
            Ref = erlang:monitor(process, Pid),
            true = ets:insert(boards_monitors, {Ref, Service, BoardId}),
            true = ets:insert(boards_table, {BoardId, #{Service => {Pid, Ref}}});
        [{BoardId}] ->
            Ref = erlang:monitor(process, Pid),
            true = ets:insert(boards_monitors, {Ref, Service, BoardId}),
            true = ets:insert(boards_table, {BoardId, #{Service => {Pid, Ref}}});
        [{BoardId, Map}] ->
            case maps:get(Service, Map, undefined) of
                undefined ->
                    Ref = erlang:monitor(process, Pid),
                    true = ets:insert(boards_monitors, {Ref, Service, BoardId}),
                    true = ets:insert(boards_table, {BoardId, Map#{Service => {Pid, Ref}}});
                {Pid, _Ref} -> already_registered
            end
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    lager:info("boards_manager_service:handle_info: ~p ~p ~p ~p", [Ref, process, Pid, Reason]),
    case ets:lookup(boards_monitors, Ref) of
        [] ->
            ok;
        [{Ref, board_sup, BoardId}] when Reason == shutdown ->
            ok = supervisor:delete_child(backend_sup, BoardId),
            true = ets:delete(boards_monitors, Ref),
            true = ets:insert(boards_table, {BoardId});
        [{Ref, Service, BoardId}] ->
            true = ets:delete(boards_monitors, Ref),
            case ets:lookup(boards_table, BoardId) of
                [] -> % board was deleted
                    ok;
                [{BoardId}] -> % board was shut down due to inactivity
                    ok;
                [{BoardId, Map}] ->
                    case maps:get(Service, Map, undefined) of
                        undefined -> ok;
                        {Pid, Ref} ->
                            ets:insert(boards_table, {BoardId, maps:remove(Service, Map)});
                        _ -> ok
                    end
            end
    end,
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
        [{BoardId}] ->
            % database like entry that has not been overwritten with Pid values so no board supervisor exists
            % this is also the case when board supervisor has terminated with normal reason
            {BoardId, Map} = create_new_board_supervisor(BoardId),
            {Pid, _MonitorRef} = maps:get(Service, Map),
            {reply, {ok, Pid}, State};
        [{BoardId, Map}] ->
            % board supervisor exists but service might not be available
            case maps:get(Service, Map, undefined) of
                undefined ->
                    {reply, service_not_available, State};
                {Pid, _MonitorRef} ->
                    {reply, {ok, Pid}, State}
            end
    end.

create_new_board_supervisor(BoardId) ->
    {ok, Supervisor} = supervisor:start_child(backend_sup, #{
        id => BoardId,
        start => {board_sup, start_link, [BoardId]},
        restart => transient,
        shutdown => 5000}
    ),
    true = ets:insert(boards_table, Entry = {BoardId, supervisor_and_children_monitors_and_pids(BoardId, Supervisor)}),
    Entry.

supervisor_and_children_monitors_and_pids(BoardId, Supervisor) ->
    Children = supervisor:which_children(Supervisor),
    MonitorRef = erlang:monitor(process, Supervisor),
    true = ets:insert(boards_monitors, {MonitorRef, board_sup, BoardId}),
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
            ets:insert(boards_monitors, {MonitorRef, Service, BoardId}),
            {BoardId, ExpectedChildren, Map#{Service => {Pid, MonitorRef}}};
        false ->
            Acc
    end.