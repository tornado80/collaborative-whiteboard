-module(scenarios_SUITE).

-include("test_common.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([user_sessions_scenario/1, large_blobs_uploading_and_downloading_scenario/1, database_persistence/1]).

all() ->
    [user_sessions_scenario, large_blobs_uploading_and_downloading_scenario, database_persistence].

init_per_testcase(_TestName, Config) ->
    AppDataDirectory = proplists:get_value(priv_dir, Config),
    application:set_env(backend, app_data_directory, AppDataDirectory, [{persistent, true}]),
    BoardInactivityTimeout = 1000,
    application:set_env(backend, board_inactivity_timeout, BoardInactivityTimeout, [{persistent, true}]),
    application:ensure_all_started(backend),
    application:ensure_all_started(gun),
    Host = "localhost",
    Port = 8080,
    [{host, Host}, {port, Port}, {board_inactivity_timeout, BoardInactivityTimeout} | Config].

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
    User1Pid = test_utility:create_user("user1", Config, BoardId, TestRunner, new, undefined),
    User2Pid = test_utility:create_user("user2", Config, BoardId, TestRunner, new, undefined),
    
    % verify welcome messages and user joined messages
    #user_state{user_id = User1Id} = test_utility:expect_welcome_user("user1"),
    #user_state{user_id = User2Id} = test_utility:expect_welcome_user("user2"),
    
    % create third user
    User3Pid = test_utility:create_user("user3", Config, BoardId, TestRunner, new, undefined),
    
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
    
    User3ContinuedPid = test_utility:create_user("user3-continued", Config, BoardId, TestRunner, continue, Token),
    
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
    
    % verify board is empty
    test_utility:verify_board_is_empty(Config, BoardId),
    
    {ok, Blob} = file:read_file(filename:join(proplists:get_value(data_dir, Config), "test.png")),
    
    % create a blob
    Headers = [{<<"content-type">>, <<"image/png">>}],
    BlobId = test_utility:post_request_to_create_blob(Config, BoardId, Headers, Blob),
    
    % verify blob is created
    Blob = test_utility:get_blob(Config, BoardId, BlobId),
    
    Config.

database_persistence(Config) ->
    % create a board
    BoardId = test_utility:post_request_to_create_board(Config),
    
    % verify board is empty
    test_utility:verify_board_is_empty(Config, BoardId),
    
    % create a user
    UserPid = test_utility:create_user("user", Config, BoardId, self(), new, undefined),
    
    % verify welcome message
    #user_state{user_id = UserId} = test_utility:expect_welcome_user("user"),
    
    
    ProposalId = utility:new_uuid(),
    UserPid ! {send, jsone:encode(#{
        <<"eventType">> => <<"boardUpdateProposed">>,
        <<"proposalId">> => ProposalId,
        <<"update">> => #{
            <<"canvasObjectType">> => <<"stickyNote">>,
            <<"operationType">> => <<"create">>,
            <<"operation">> => #{
                <<"canvasObjectOperationType">> => <<"createStickyNote">>,
                <<"text">> => <<"Hello, World!">>
            }
        }
    })},
    
    % expect board update success
    CanvasObjectId = test_utility:expect_board_update_succeeded("user", ProposalId),
    
    % close the connection
    UserPid ! close_ws,
    
    % wait for board controller to shut down
    timer:sleep(proplists:get_value(board_inactivity_timeout, Config) + 2000),
    
    % verify board
    Board = test_utility:get_board_state(Config, BoardId),
    [] = proplists:get_value(<<"onlineUsers">>, Board), % verifies new board has been
    [Object] = proplists:get_value(<<"canvasObjects">>, Board), % verifies database persistence
    CanvasObjectId = proplists:get_value(<<"id">>, Object), % verifies database persistence
    0 = proplists:get_value(<<"lastUpdateId">>, Board),  % verifies new board has been
    
    Config.