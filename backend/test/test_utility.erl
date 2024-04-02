-module(test_utility).

-include("test_common.hrl").

-export([post_request_to_create_board/1, verify_board_is_empty/2,
    create_user/6, expect_welcome_user/2, expect_user_joined/3,
    expect_user_left/3, get_board_state/2, post_request_to_create_blob/4, get_blob/3,
    expect_board_update_succeeded/3, verify_blob_does_not_exist/3,
    expect_reservation_succeeded/3, expect_reservation_cancelled/3,
    expect_board_updated/5, expect_canvas_object_reserved/5, expect_reservation_expired/3,
    expect_board_update_failed/3, expect_reservation_failed/3, send_msg_to_user/2,
    expect_board_update_succeeded_and_received_time/4,
    expect_board_updated_and_received_time/6,
    expect_welcome_user_with_timeout/3]).

open_connection_with_server(Config) ->
    {ok, Pid} = gun:open(proplists:get_value(host, Config), proplists:get_value(port, Config)),
    Pid.

open_ws_connection_with_server(Config, State = #user_state{board_id = BoardId}) ->
    Pid = open_connection_with_server(Config),
    MonitorRef = erlang:monitor(process, Pid),
    log_user_action(State, "connected to server"),
    StreamRef = gun:ws_upgrade(Pid, <<"/api/ws/boards/", BoardId/binary>>),
    receive
        {gun_upgrade, Pid, StreamRef, [<<"websocket">>], _Headers} ->
            log_user_action(State, "upgraded connection to websocket"),
            State#user_state{conn_pid = Pid, stream_ref = StreamRef, monitor_ref = MonitorRef}
    after 100 ->
        log_user_action(State, "failed to upgrade connection to websocket"),
        exit(gun_upgrade_failed)
    end.

post_request_to_create_board(Config) ->
    Pid = open_connection_with_server(Config),
    StreamRef = gun:post(Pid, "/api/rest/boards", []),
    {response, nofin, 201, _Headers} = gun:await(Pid, StreamRef),
    {ok, Body} = gun:await_body(Pid, StreamRef),
    BoardId = proplists:get_value(<<"boardId">>, jsone:decode(Body, [{object_format, proplist}])),
    gun:close(Pid),
    BoardId.

post_request_to_create_blob(Config, BoardId, Headers, Blob) ->
    Pid = open_connection_with_server(Config),
    StreamRef = gun:post(Pid, <<"/api/rest/boards/", BoardId/binary, "/blobs">>, Headers, Blob),
    {response, nofin, 201, _Headers} = gun:await(Pid, StreamRef),
    {ok, Body} = gun:await_body(Pid, StreamRef),
    BlobId = proplists:get_value(<<"blobId">>, jsone:decode(Body, [{object_format, proplist}])),
    gun:close(Pid),
    BlobId.

get_blob(Config, BoardId, BlobId) ->
    Pid = open_connection_with_server(Config),
    StreamRef = gun:get(Pid, <<"/api/rest/boards/", BoardId/binary, "/blobs/", BlobId/binary>>),
    {response, nofin, 200, _Headers} = gun:await(Pid, StreamRef),
    {ok, Body} = gun:await_body(Pid, StreamRef),
    gun:close(Pid),
    Body.

verify_blob_does_not_exist(Config, BoardId, BlobId) ->
    Pid = open_connection_with_server(Config),
    StreamRef = gun:get(Pid, <<"/api/rest/boards/", BoardId/binary, "/blobs/", BlobId/binary>>),
    {response, _, 404, _Headers} = gun:await(Pid, StreamRef),
    gun:close(Pid).

log_user_action(State, Action) ->
    ct:log(io_lib:format("User ~p [~p]: ~p", [State#user_state.test_name, State#user_state.server_name,
        lists:flatten(Action)])).

create_user(Name, Config, BoardId, TestRunner, SessionType, SessionToken) ->
    {Pid, _Monitor} = spawn_monitor(fun() -> do_create_user(Name, Config, BoardId, TestRunner, SessionType, SessionToken) end),
    Pid.
    
do_create_user(Name, Config, BoardId, TestRunner, SessionType, SessionToken) ->
    State = #user_state{test_name = Name, board_id = BoardId, test_runner = TestRunner},
    log_user_action(State, "created"),
    State1 = open_ws_connection_with_server(Config, State),
    BeginMessage = case SessionType of
        new ->
            jsone:encode(#{
                <<"eventType">> => <<"begin">>,
                <<"sessionType">> => <<"new">>
            });
        continue ->
            jsone:encode(#{
                <<"eventType">> => <<"begin">>,
                <<"sessionType">> => <<"continue">>,
                <<"sessionToken">> => SessionToken
            })
    end,
    gun:ws_send(State1#user_state.conn_pid, State1#user_state.stream_ref, {text, BeginMessage}),
    log_user_action(State1, "sent begin message"),
    log_user_action(State1, "entered user loop"),
    user_loop(State1).

user_loop(State = #user_state{conn_pid = Pid, stream_ref = StreamRef, test_name = TestName, monitor_ref = MonitorRef, test_runner = TestRunner}) ->
    NewState = receive
        {send, Msg} ->
            gun:ws_send(Pid, StreamRef, {text, Msg}),
            log_user_action(State, io_lib:format("sent message ~p", [Msg])),
            State;
        {send, Msg, From, Ref} ->
            SentTime = os:system_time(millisecond),
            gun:ws_send(Pid, StreamRef, {text, Msg}),
            From ! {sent, Ref, SentTime},
            log_user_action(State, io_lib:format("sent message ~p", [Msg])),
            State;
        {set_test_runner, NewTestRunner} ->
            State#user_state{test_runner = NewTestRunner};
        {gun_ws, Pid, StreamRef, {text, Json}} ->
            When = os:system_time(millisecond),
            PropList = jsone:decode(Json, [{object_format, proplist}]),
            log_user_action(State, io_lib:format("received message ~p", [Json])),
            EventType = proplists:get_value(<<"eventType">>, PropList),
            case EventType of
                <<"welcomeUser">> ->
                    State1 = State#user_state{
                       server_name = proplists:get_value(<<"userName">>, PropList),
                       user_id = proplists:get_value(<<"userId">>, PropList),
                       session_token = proplists:get_value(<<"sessionToken">>, PropList)},
                    TestRunner ! {welcome_user, TestName, State1, When},
                    State1;
                <<"userJoined">> ->
                    UserId = proplists:get_value(<<"userId">>, PropList),
                    TestRunner ! {user_joined, TestName, UserId, When},
                    State;
                <<"userLeft">> ->
                    UserId = proplists:get_value(<<"userId">>, PropList),
                    TestRunner ! {user_left, TestName, UserId, When},
                    State;
                <<"boardUpdateSucceeded">> ->
                    Update = proplists:get_value(<<"update">>, PropList),
                    ProposalId = proplists:get_value(<<"proposalId">>, PropList),
                    UpdateId = proplists:get_value(<<"updateId">>, PropList),
                    CanvasObjectId = proplists:get_value(<<"canvasObjectId">>, Update),
                    TestRunner ! {board_update_succeeded, TestName, ProposalId, CanvasObjectId, UpdateId, When},
                    State;
                <<"boardUpdateFailed">> ->
                    ProposalId = proplists:get_value(<<"proposalId">>, PropList),
                    ErrorMessage = proplists:get_value(<<"errorMessage">>, PropList),
                    TestRunner ! {board_update_failed, TestName, ProposalId, ErrorMessage, When},
                    State;
                <<"boardUpdated">> ->
                    Update = proplists:get_value(<<"update">>, PropList),
                    UpdateId = proplists:get_value(<<"updateId">>, PropList),
                    UserId = proplists:get_value(<<"userId">>, PropList),
                    CanvasObjectId = proplists:get_value(<<"canvasObjectId">>, Update),
                    CanvasObjectType = proplists:get_value(<<"canvasObjectType">>, Update),
                    OperationType = proplists:get_value(<<"operationType">>, Update),
                    Operation = proplists:get_value(<<"operation">>, Update),
                    TestRunner ! {board_updated, TestName, UpdateId, UserId, CanvasObjectId, CanvasObjectType, OperationType, Operation, When},
                    State;
                <<"reservationProposalSucceeded">> ->
                    ProposalId = proplists:get_value(<<"proposalId">>, PropList),
                    ReservationId = proplists:get_value(<<"reservationId">>, PropList),
                    ExpirationTimestamp = proplists:get_value(<<"expirationTimestamp">>, PropList),
                    TestRunner ! {reservation_succeeded, TestName, ProposalId, ReservationId, ExpirationTimestamp, When},
                    State;
                <<"reservationCancelled">> ->
                    ReservationId = proplists:get_value(<<"reservationId">>, PropList),
                    TestRunner ! {reservation_cancelled, TestName, ReservationId, When},
                    State;
                <<"reservationExpired">> ->
                    ReservationId = proplists:get_value(<<"reservationId">>, PropList),
                    TestRunner ! {reservation_expired, TestName, ReservationId, When},
                    State;
                <<"reservationProposalFailed">> ->
                    ProposalId = proplists:get_value(<<"proposalId">>, PropList),
                    ErrorMessage = proplists:get_value(<<"errorMessage">>, PropList),
                    TestRunner ! {reservation_failed, TestName, ProposalId, ErrorMessage, When},
                    State;
                <<"canvasObjectReserved">> ->
                    CanvasObjectId = proplists:get_value(<<"canvasObjectId">>, PropList),
                    ReservationId = proplists:get_value(<<"reservationId">>, PropList),
                    ExpirationTimestamp = proplists:get_value(<<"expirationTimestamp">>, PropList),
                    UserId = proplists:get_value(<<"userId">>, PropList),
                    TestRunner ! {canvas_object_reserved, TestName, CanvasObjectId, ReservationId, ExpirationTimestamp, UserId, When},
                    State;
                _ ->
                    TestRunner ! {unknown_event, TestName, State, EventType, PropList, When},
                    State
            end;
        {gun_down, Pid, ws, Reason, [StreamRef]} ->
            log_user_action(State, io_lib:format("connection down: ~p", [Reason])),
            exit(connection_down);
        {gun_error, Pid, StreamRef, Reason} ->
            log_user_action(State, io_lib:format("stream error: ~p", [Reason])),
            exit(stream_error);
        {gun_error, Pid, Reason} ->
            log_user_action(State, io_lib:format("connection error: ~p", [Reason])),
            exit(connection_error);
        {'DOWN', MonitorRef, process, Pid, Reason} ->
            log_user_action(State, io_lib:format("gun crashed: ~p", [Reason])),
            exit(gun_crashed);
        die ->
            log_user_action(State, "dying"),
            exit(Pid, kill),
            exit(died);
        close_abruptly ->
            log_user_action(State, "abruptly closing connection"),
            gun:close(Pid),
            exit(closed_abruptly);
        close_ws ->
            log_user_action(State, "sending close frame"),
            gun:ws_send(Pid, StreamRef, close),
            exit(normal)
    end,
    user_loop(NewState).

send_msg_to_user(Pid, Msg) ->
    Ref = erlang:make_ref(),
    Pid ! {send, Msg, self(), Ref},
    receive {sent, Ref, SentTime} -> SentTime end.

get_board_state(Config, BoardId) ->
    Pid = open_connection_with_server(Config),
    StreamRef = gun:get(Pid, <<"/api/rest/boards/", BoardId/binary>>),
    {response, nofin, 200, _Headers} = gun:await(Pid, StreamRef),
    {ok, Body} = gun:await_body(Pid, StreamRef),
    Board = jsone:decode(Body, [{object_format, proplist}]),
    gun:close(Pid),
    Board.

verify_board_is_empty(Config, BoardId) ->
    Board = get_board_state(Config, BoardId),
    [] = proplists:get_value(<<"canvasObjects">>, Board),
    0 = proplists:get_value(<<"lastUpdateId">>, Board),
    BoardId = proplists:get_value(<<"id">>, Board),
    [] = proplists:get_value(<<"onlineUsers">>, Board).

expect_welcome_user_with_timeout(TestUserName, TestUserPid, Timeout) ->
    receive
        {welcome_user, TestUserName, State, _When} -> State;
        {'DOWN', _, process, TestUserPid, Reason} -> exit({user_down, Reason})
    after
        Timeout -> exit(welcome_user_not_received)
    end.

expect_welcome_user(TestUserName, TestUserPid) ->
    expect_welcome_user_with_timeout(TestUserName, TestUserPid, 100).

expect_user_joined(TestUserName, TestUserPid, JoinedUserId) ->
    receive
        {user_joined, TestUserName, JoinedUserId, _When} -> ok;
        {'DOWN', _, process, TestUserPid, Reason} -> exit({user_down, TestUserName, Reason})
    after
        100 -> exit(user_joined_not_received)
    end.

expect_user_left(TestUserName, TestUserPid, LeftUserId) ->
    receive
        {user_left, TestUserName, LeftUserId, _When} -> ok;
        {'DOWN', _, process, TestUserPid, Reason} -> exit({user_down, TestUserName, Reason})
    after
        100 -> exit(user_left_not_received)
    end.

expect_board_update_succeeded_and_received_time(TestUserName, TestUserPid, ProposalId, Timeout) ->
    receive
        {board_update_succeeded, TestUserName, ProposalId, CanvasObjectId, UpdateId, When} -> {CanvasObjectId, UpdateId, When};
        {'DOWN', _, process, TestUserPid, Reason} -> exit({user_down, TestUserName, Reason})
    after
        Timeout -> exit(board_update_succeeded_not_received)
    end.

expect_board_update_succeeded(TestUserName, TestUserPid, ProposalId) ->
    {CanvasObjectId, UpdateId, _When} = expect_board_update_succeeded_and_received_time(TestUserName, TestUserPid, ProposalId, 100),
    {CanvasObjectId, UpdateId}.

expect_reservation_succeeded(TestUserName, TestUserPid, ProposalId) ->
    receive
        {reservation_succeeded, TestUserName, ProposalId, ReservationId, _ExpirationTimestamp, _When} -> ReservationId;
        {'DOWN', _, process, TestUserPid, Reason} -> exit({user_down, TestUserName, Reason})
    after
        100 -> exit(reservation_succeeded_not_received)
    end.

expect_reservation_cancelled(TestUserName, TestUserPid, ReservationId) ->
    receive
        {reservation_cancelled, TestUserName, ReservationId, _When} -> ok;
        {'DOWN', _, process, TestUserPid, Reason} -> exit({user_down, TestUserName, Reason})
    after
        100 -> exit(reservation_cancelled_not_received)
    end.

expect_board_updated_and_received_time(TestUserName, TestUserPid, UpdateId, UserId, CanvasObjectId, Timeout) ->
    receive
        {board_updated, TestUserName, UpdateId, UserId, CanvasObjectId, _CanvasObjectType, OperationType, Operation, When} -> {OperationType, Operation, When};
        {'DOWN', _, process, TestUserPid, Reason} -> exit({user_down, TestUserName, Reason})
    after
        Timeout -> exit(board_updated_not_received)
    end.

expect_board_updated(TestUserName, TestUserPid, UpdateId, UserId, CanvasObjectId) ->
    {OperationType, Operation, When} = expect_board_updated_and_received_time(TestUserName, TestUserPid, UpdateId, UserId, CanvasObjectId, 100),
    {OperationType, Operation}.

expect_canvas_object_reserved(TestUserName, TestUserPid, CanvasObjectId, ReservationId, UserId) ->
    receive
        {canvas_object_reserved, TestUserName, CanvasObjectId, ReservationId, _ExpirationTimestamp, UserId, _When} -> ok;
        {'DOWN', _, process, TestUserPid, Reason} -> exit({user_down, TestUserName, Reason})
    after
        100 -> exit(canvas_object_reserved_not_received)
    end.

expect_reservation_expired(TestUserName, TestUserPid, ReservationId) ->
    receive
        {reservation_expired, TestUserName, ReservationId, _When} -> ok;
        {'DOWN', _, process, TestUserPid, Reason} -> exit({user_down, TestUserName, Reason})
    after
        100 -> exit(reservation_expired_not_received)
    end.

expect_reservation_failed(TestUserName, TestUserPid, ProposalId) ->
    receive
        {reservation_failed, TestUserName, ProposalId, _ErrorMessage, _When} -> ok;
        {'DOWN', _, process, TestUserPid, Reason} -> exit({user_down, TestUserName, Reason})
    after
        100 -> exit(reservation_failed_not_received)
    end.

expect_board_update_failed(TestUserName, TestUserPid, ProposalId) ->
    receive
        {board_update_failed, TestUserName, ProposalId, _ErrorMessage, _When} -> ok;
        {'DOWN', _, process, TestUserPid, Reason} -> exit({user_down, TestUserName, Reason})
    after
        100 -> exit(board_update_failed_not_received)
    end.