-module(scenarios_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("test_common.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([user_sessions_scenario/1, large_blobs_uploading_and_downloading_scenario/1]).

all() ->
    [user_sessions_scenario, large_blobs_uploading_and_downloading_scenario].

init_per_testcase(_TestName, Config) ->
    application:ensure_all_started(backend),
    application:ensure_all_started(gun),
    Host = "localhost",
    Port = 8080,
    [{host, Host}, {port, Port} | Config].

end_per_testcase(_TestName, Config) ->
    application:stop(backend),
    application:stop(gun),
    Config.

user_sessions_scenario(Config) ->
    % create a board
    BoardId = test_utility:post_request_to_create_board(Config),
    
    % verify board is empty
    test_utility:verify_board_is_empty(Config, BoardId),
    
    TestRunner = self(),
    
    % create 2 users
    User1Pid = spawn_link(fun() -> test_utility:create_user("user1", Config, BoardId, TestRunner, new, undefined) end),
    User2Pid = spawn_link(fun() -> test_utility:create_user("user2", Config, BoardId, TestRunner, new, undefined) end),
    
    % verify welcome messages and user joined messages
    #user_state{user_id = User1Id} = test_utility:expect_welcome_user("user1"),
    #user_state{user_id = User2Id} = test_utility:expect_welcome_user("user2"),
    
    % create third user
    User3Pid = spawn(fun() -> test_utility:create_user("user3", Config, BoardId, TestRunner, new, undefined) end),
    
    % wait for welcome message of third user
    #user_state{user_id = User3Id, session_token = Token} = test_utility:expect_welcome_user("user3"),
    
    % wait for user joined messages of other users and verify it
    test_utility:expect_user_joined("user1", User3Id),
    test_utility:expect_user_joined("user2", User3Id),
    
    % verify board
    Board = test_utility:get_board_state(Config, BoardId),
    OnlineUsers = proplists:get_value(<<"onlineUsers">>, Board),
    lists:foreach(
        fun(User) ->
            true = lists:member(proplists:get_value(<<"id">>, User), [User1Id, User2Id, User3Id])
        end,
        OnlineUsers
    ),
    
    User3Pid ! die,
    
    test_utility:expect_user_left("user1", User3Id),
    test_utility:expect_user_left("user2", User3Id),
    
    User3ContinuedPid = spawn(fun() -> test_utility:create_user("user3-continued", Config, BoardId, TestRunner, continue, Token) end),
    
    #user_state{user_id = User3Id, session_token = Token} = test_utility:expect_welcome_user("user3-continued"),
    
    test_utility:expect_user_joined("user1", User3Id),
    test_utility:expect_user_joined("user2", User3Id),
    
    User3ContinuedPid ! close_abruptly,
    
    test_utility:expect_user_left("user1", User3Id),
    test_utility:expect_user_left("user2", User3Id),
    
    % close all connections
    User1Pid ! close_ws,
    User2Pid ! close_ws,
    
    [{board_id, BoardId} | Config].

large_blobs_uploading_and_downloading_scenario(Config) ->
    % create a board
    BoardId = test_utility:post_request_to_create_board(Config),
    
    
    Config.
