-module(scenarios_SUITE).

-include_lib("common_test/include/ct.hrl").

-record(user_state, {
    test_name,
    server_name = undefined,
    supervisor,
    conn_pid,
    stream_ref,
    board_id,
    user_id,
    session_token
}).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([scenario1/1]).

all() ->
    [scenario1].

init_per_testcase(scenario1, Config) ->
    %application:ensure_all_started(backend),
    application:ensure_all_started(gun),
    Host = "localhost",
    Port = 8080,
    [{host, Host}, {port, Port} | Config].

end_per_testcase(scenario1, Config) ->
    %application:stop(backend),
    application:stop(gun),
    Config.

open_connection_with_server(Config) ->
    {ok, Pid} = gun:open(?config(host, Config), ?config(port, Config)),
    Pid.

open_ws_connection_with_server(Config, State = #user_state{board_id = BoardId}) ->
    Pid = open_connection_with_server(Config),
    log_user_action(State, "connected to server"),
    StreamRef = gun:ws_upgrade(Pid, <<"/api/ws/boards/", BoardId/binary>>),
    receive
        {gun_upgrade, Pid, StreamRef, [<<"websocket">>], _Headers} ->
            log_user_action(State, "upgraded connection to websocket"),
            State#user_state{conn_pid = Pid, stream_ref = StreamRef}
    end.

post_request_to_create_board(Config) ->
    Pid = open_connection_with_server(Config),
    StreamRef = gun:post(Pid, "/api/rest/boards", [{<<"content-type">>, <<"application/json">>}]),
    {response, nofin, 201, _Headers} = gun:await(Pid, StreamRef),
    {ok, Body} = gun:await_body(Pid, StreamRef),
    BoardId = proplists:get_value(<<"boardId">>, jsone:decode(Body, [{object_format, proplist}])),
    gun:close(Pid),
    BoardId.

log_user_action(State, Action) ->
    ct:print(io_lib:format("User ~p: [~p] ~p", [State#user_state.test_name, State#user_state.server_name,
        lists:flatten(Action)])).

create_user(Name, Config, BoardId, Supervisor, SessionType, SessionToken) ->
    State = #user_state{test_name = Name, board_id = BoardId, supervisor = Supervisor},
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
    user_loop(State1, Supervisor).

user_loop(State = #user_state{conn_pid = Pid, stream_ref = StreamRef, test_name = TestName}, Supervisor) ->
    NewState = receive
        {send, Msg} ->
            gun:ws_send(Pid, StreamRef, {text, Msg}),
            log_user_action(State, io_lib:format("sent message ~p", [Msg])),
            State;
        {gun_ws, Pid, StreamRef, {text, Json}} ->
            PropList = jsone:decode(Json, [{object_format, proplist}]),
            log_user_action(State, io_lib:format("received message ~p", [Json])),
            EventType = proplists:get_value(<<"eventType">>, PropList),
            case EventType of
                <<"welcomeUser">> ->
                    State1 = State#user_state{
                        server_name = proplists:get_value(<<"userName">>, PropList),
                        user_id = proplists:get_value(<<"userId">>, PropList),
                        session_token = proplists:get_value(<<"sessionToken">>, PropList)},
                    Supervisor ! {welcome_user, TestName, State1},
                    State1;
                <<"userJoined">> ->
                    UserId = proplists:get_value(<<"userId">>, PropList),
                    Supervisor ! {user_joined, TestName, UserId},
                    State;
                <<"userLeft">> ->
                    UserId = proplists:get_value(<<"userId">>, PropList),
                    Supervisor ! {user_left, TestName, UserId},
                    State;
                _ ->
                    Supervisor ! {user_event, TestName, State, EventType, PropList},
                    State
            end;
        die ->
            log_user_action(State, "dying"),
            exit(Pid, kill),
            exit(normal);
        close_abruptly ->
            log_user_action(State, "abruptly closing connection"),
            gun:close(Pid),
            exit(normal);
        close_ws ->
            log_user_action(State, "sending close frame"),
            gun:ws_send(Pid, StreamRef, close),
            exit(normal)
    end,
    user_loop(NewState, Supervisor).

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


expect_welcome_user(TestUserName) ->
    receive
        {welcome_user, TestUserName, State} -> State
    end.

expect_user_joined(TestUserName, UserId) ->
    receive
        {user_joined, TestUserName, UserId} -> ok
    end.

expect_user_left(TestUserName, UserId) ->
    receive
        {user_left, TestUserName, UserId} -> ok
    end.

scenario1(Config) ->
    % create a board
    BoardId = post_request_to_create_board(Config),
    
    % verify board is empty
    verify_board_is_empty(Config, BoardId),
    
    % create 10 users
    TestRunner = self(),
    Users = lists:foldl(
        fun(Name, Map) ->
            Map#{Name => spawn_link(fun() -> create_user(Name, Config, BoardId, TestRunner, new, undefined) end)}
        end,
        #{},
        lists:seq(1, 2)
    ),
    
    % verify welcome messages and user joined messages
    #user_state{user_id = U1} = expect_welcome_user(1),
    #user_state{user_id = U2} = expect_welcome_user(2),
    
    % create third user
    Pid3 = spawn(fun() -> create_user(3, Config, BoardId, TestRunner, new, undefined) end),
    
    % wait for welcome message of third user
    #user_state{user_id = U3, session_token = Token} = expect_welcome_user(3),
    
    % wait for user joined messages of other users and verify it
    expect_user_joined(1, U3),
    expect_user_joined(2, U3),
    
    % verify board
    Board = get_board_state(Config, BoardId),
    OnlineUsers = proplists:get_value(<<"onlineUsers">>, Board),
    lists:foreach(
        fun(User) ->
            true = lists:member(proplists:get_value(<<"id">>, User), [U1, U2, U3])
        end,
        OnlineUsers
    ),
    
    Pid3 ! die,
    
    expect_user_left(1, U3),
    expect_user_left(2, U3),
    
    Pid4 = spawn(fun() -> create_user(4, Config, BoardId, TestRunner, continue, Token) end),
    
    #user_state{user_id = U3, session_token = Token} = expect_welcome_user(4),
    
    expect_user_joined(1, U3),
    expect_user_joined(2, U3),
    
    Pid4 ! close_abruptly,
    
    expect_user_left(1, U3),
    expect_user_left(2, U3),
    
    % close all connections
    maps:foreach(
        fun(Name, Pid) ->
            Pid ! close_ws
        end,
        Users
    ),
    
    [{board_id, BoardId} | Config].
