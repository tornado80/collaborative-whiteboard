-module(scenarios_SUITE).

-include("test_common.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
    user_sessions_scenario/1,
    large_blobs_uploading_and_downloading_scenario/1,
    database_persistence/1,
    blobs_database_persistence/1,
    update_sticky_note_scenario/1,
    extend_reservation/1,
    undo_redo_scenario/1,
    benchmark1/1,
    benchmark2/1
]).

all() ->
    [
        user_sessions_scenario,
        large_blobs_uploading_and_downloading_scenario,
        database_persistence, blobs_database_persistence,
        update_sticky_note_scenario,
        extend_reservation,
        undo_redo_scenario,
        benchmark1,
        benchmark2
    ].

init_per_testcase(_TestName, Config) ->
    AppDataDirectory = proplists:get_value(priv_dir, Config),
    application:set_env(backend, app_data_directory, AppDataDirectory, [{persistent, true}]),
    application:ensure_all_started(backend),
    application:ensure_all_started(gun),
    Host = "localhost",
    Port = 8080,
    [{host, Host}, {port, Port} | Config].

end_per_testcase(_TestName, Config) ->
    application:stop(backend),
    application:stop(gun).

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
    #user_state{user_id = User1Id} = test_utility:expect_welcome_user("user1", User1Pid),
    #user_state{user_id = User2Id} = test_utility:expect_welcome_user("user2", User2Pid),
    
    % create third user
    User3Pid = test_utility:create_user("user3", Config, BoardId, TestRunner, new, undefined),
    
    % wait for welcome message of third user
    #user_state{user_id = User3Id, session_token = Token} = test_utility:expect_welcome_user("user3", User3Pid),
    
    % wait for user joined messages of other users and verify it
    test_utility:expect_user_joined("user1", User1Pid, User3Id),
    test_utility:expect_user_joined("user2", User2Pid, User3Id),
    
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
    
    test_utility:expect_user_left("user1", User1Pid, User3Id),
    test_utility:expect_user_left("user2", User2Pid, User3Id),
    
    User3ContinuedPid = test_utility:create_user("user3-continued", Config, BoardId, TestRunner, continue, Token),
    
    #user_state{user_id = User3Id, session_token = Token} = test_utility:expect_welcome_user("user3-continued", User3ContinuedPid),
    
    test_utility:expect_user_joined("user1", User1Pid, User3Id),
    test_utility:expect_user_joined("user2", User2Pid, User3Id),
    
    User3ContinuedPid ! close_abruptly,
    
    test_utility:expect_user_left("user1", User1Pid, User3Id),
    test_utility:expect_user_left("user2", User2Pid, User3Id),
    
    % close all connections
    User1Pid ! close_ws,
    User2Pid ! close_ws.

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
    Blob = test_utility:get_blob(Config, BoardId, BlobId).

database_persistence(Config) ->
    % this decreases the inactivity timer to shutdown the board controller faster
    BoardInactivityTimeout = 1000,
    application:set_env(backend, board_inactivity_timeout, BoardInactivityTimeout, [{persistent, true}]),
    
    % create a board
    BoardId = test_utility:post_request_to_create_board(Config),
    
    % verify board is empty
    test_utility:verify_board_is_empty(Config, BoardId),
    
    % create a user
    UserPid = test_utility:create_user("user", Config, BoardId, self(), new, undefined),
    
    % verify welcome message
    #user_state{user_id = UserId} = test_utility:expect_welcome_user("user", UserPid),
    
    % create sticky note
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
    {CanvasObjectId, _UpdatedId} = test_utility:expect_board_update_succeeded("user", UserPid, ProposalId),
    
    % close the connection
    UserPid ! close_ws,
    
    % wait for board controller to shut down
    timer:sleep(BoardInactivityTimeout + 500),
    
    % verify board
    Board = test_utility:get_board_state(Config, BoardId),
    [] = proplists:get_value(<<"onlineUsers">>, Board), % verifies new board has been
    [Object] = proplists:get_value(<<"canvasObjects">>, Board), % verifies database persistence
    CanvasObjectId = proplists:get_value(<<"id">>, Object), % verifies database persistence
    0 = proplists:get_value(<<"lastUpdateId">>, Board).  % verifies new board has been

blobs_database_persistence(Config) ->
    % this decreases the inactivity timer to shutdown the board controller faster
    BoardInactivityTimeout = 1000,
    application:set_env(backend, board_inactivity_timeout, BoardInactivityTimeout, [{persistent, true}]),
    
    % create a board
    BoardId = test_utility:post_request_to_create_board(Config),
    
    % verify board is empty
    test_utility:verify_board_is_empty(Config, BoardId),
    
    % create a user
    UserPid = test_utility:create_user("user", Config, BoardId, self(), new, undefined),
    
    % verify welcome message
    #user_state{user_id = UserId} = test_utility:expect_welcome_user("user", UserPid),
    
    % create a blob
    {ok, Blob} = file:read_file(filename:join(proplists:get_value(data_dir, Config), "test.png")),
    Headers = [{<<"content-type">>, <<"image/png">>}],
    BlobId = test_utility:post_request_to_create_blob(Config, BoardId, Headers, Blob),
    
    % create another blob to tests database service cleanup when a blob is not referenced by images
    Blob2Id = test_utility:post_request_to_create_blob(Config, BoardId, Headers, Blob),
    
    % verify blob is created
    Blob = test_utility:get_blob(Config, BoardId, BlobId),
    
    % create image
    ProposalId = utility:new_uuid(),
    UserPid ! {send, jsone:encode(#{
        <<"eventType">> => <<"boardUpdateProposed">>,
        <<"proposalId">> => ProposalId,
        <<"update">> => #{
            <<"canvasObjectType">> => <<"image">>,
            <<"operationType">> => <<"create">>,
            <<"operation">> => #{
                <<"canvasObjectOperationType">> => <<"createImage">>,
                <<"blobId">> => BlobId
            }
        }
    })},
    
    % expect board update success
    {CanvasObjectId, _UpdatedId} = test_utility:expect_board_update_succeeded("user", UserPid, ProposalId),
    
    % close the connection
    UserPid ! close_ws,
    
    % wait for board controller to shut down
    timer:sleep(BoardInactivityTimeout + 500),
    
    % verify board
    Board = test_utility:get_board_state(Config, BoardId),
    [] = proplists:get_value(<<"onlineUsers">>, Board), % verifies new board has been
    [Object] = proplists:get_value(<<"canvasObjects">>, Board), % verifies database persistence
    CanvasObjectId = proplists:get_value(<<"id">>, Object), % verifies database persistence
    0 = proplists:get_value(<<"lastUpdateId">>, Board),  % verifies new board has been
    
    % verify blob is created
    Blob = test_utility:get_blob(Config, BoardId, BlobId),
    
    % verify blob is deleted
    test_utility:verify_blob_does_not_exist(Config, BoardId, Blob2Id).
    
update_sticky_note_scenario(Config) ->
    % create a board
    BoardId = test_utility:post_request_to_create_board(Config),
    
    % verify board is empty
    test_utility:verify_board_is_empty(Config, BoardId),
    
    % create a user
    UserPid = test_utility:create_user("user", Config, BoardId, self(), new, undefined),
    User2Pid = test_utility:create_user("user2", Config, BoardId, self(), new, undefined),
    
    % verify welcome message
    #user_state{user_id = UserId} = test_utility:expect_welcome_user("user", UserPid),
    #user_state{user_id = User2Id} = test_utility:expect_welcome_user("user2", User2Pid),
    
    % create sticky note
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
    {CanvasObjectId, UpdatedId} = test_utility:expect_board_update_succeeded("user", UserPid, ProposalId),
    
    % expect board updated
    test_utility:expect_board_updated("user2", User2Pid, UpdatedId, UserId, CanvasObjectId),
    
    % reserve object for update
    ProposalId2 = utility:new_uuid(),
    UserPid ! {send, jsone:encode(#{
        <<"eventType">> => <<"reservationProposed">>,
        <<"canvasObjectId">> => CanvasObjectId,
        <<"proposalId">> => ProposalId2
    })},
    
    % expect reservation success
    ReservationId = test_utility:expect_reservation_succeeded("user", UserPid, ProposalId2),
    
    % expect object reserved
    test_utility:expect_canvas_object_reserved("user2", User2Pid, CanvasObjectId, ReservationId, UserId),
    
    % try to reserve object by user2
    User2ProposalId1 = utility:new_uuid(),
    User2Pid ! {send, jsone:encode(#{
        <<"eventType">> => <<"reservationProposed">>,
        <<"canvasObjectId">> => CanvasObjectId,
        <<"proposalId">> => User2ProposalId1
    })},
    
    % expect reservation failed
    test_utility:expect_reservation_failed("user2", User2Pid, User2ProposalId1),
    
    % try to update object by user2
    User2ProposalId2 = utility:new_uuid(),
    User2Pid ! {send, jsone:encode(#{
        <<"eventType">> => <<"boardUpdateProposed">>,
        <<"proposalId">> => User2ProposalId2,
        <<"update">> => #{
            <<"canvasObjectType">> => <<"stickyNote">>,
            <<"operationType">> => <<"update">>,
            <<"canvasObjectId">> => CanvasObjectId,
            <<"operation">> => #{
                <<"canvasObjectOperationType">> => <<"updateStickyNote">>,
                <<"text">> => <<"Hello, World! User 2 Updated!">>
            }
        }
    })},
    
    % expect update failed
    test_utility:expect_board_update_failed("user2", User2Pid, User2ProposalId2),
    
    % update sticky note
    ProposalId3 = utility:new_uuid(),
    UserPid ! {send, jsone:encode(#{
        <<"eventType">> => <<"boardUpdateProposed">>,
        <<"proposalId">> => ProposalId3,
        <<"update">> => #{
            <<"canvasObjectType">> => <<"stickyNote">>,
            <<"operationType">> => <<"update">>,
            <<"canvasObjectId">> => CanvasObjectId,
            <<"operation">> => #{
                <<"canvasObjectOperationType">> => <<"updateStickyNote">>,
                <<"text">> => <<"Hello, World! Updated!">>
            }
        }
    })},
    
    % expect board update success
    {CanvasObjectId, UpdatedId2} = test_utility:expect_board_update_succeeded("user", UserPid, ProposalId3),
    
    % expect board updated
    test_utility:expect_board_updated("user2", User2Pid, UpdatedId2, UserId, CanvasObjectId),
    
    % cancel reservation
    UserPid ! {send, jsone:encode(#{
        <<"eventType">> => <<"reservationCancellationRequested">>,
        <<"reservationId">> => ReservationId
    })},
    
    % expect reservation cancelled
    test_utility:expect_reservation_cancelled("user", UserPid, ReservationId),
    test_utility:expect_reservation_cancelled("user2", User2Pid, ReservationId),
    
    % close the connection
    UserPid ! close_ws,
    User2Pid ! close_ws.

extend_reservation(Config) ->
    ObjectReservationPeriod = 1000,
    application:set_env(backend, object_reservation_period, ObjectReservationPeriod, [{persistent, true}]),
    
    % create a board
    BoardId = test_utility:post_request_to_create_board(Config),
    
    % verify board is empty
    test_utility:verify_board_is_empty(Config, BoardId),
    
    % create a user
    UserPid = test_utility:create_user("user", Config, BoardId, self(), new, undefined),
    
    % verify welcome message
    #user_state{user_id = UserId} = test_utility:expect_welcome_user("user", UserPid),
    
    % create sticky note
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
    {CanvasObjectId, _UpdatedId} = test_utility:expect_board_update_succeeded("user", UserPid, ProposalId),
    
    % reserve object for update
    ProposalId2 = utility:new_uuid(),
    UserPid ! {send, jsone:encode(#{
        <<"eventType">> => <<"reservationProposed">>,
        <<"canvasObjectId">> => CanvasObjectId,
        <<"proposalId">> => ProposalId2
    })},
    
    % expect reservation success
    ReservationId = test_utility:expect_reservation_succeeded("user", UserPid, ProposalId2),
    
    % extend reservation
    ProposalId3 = utility:new_uuid(),
    UserPid ! {send, jsone:encode(#{
        <<"eventType">> => <<"reservationProposed">>,
        <<"canvasObjectId">> => CanvasObjectId,
        <<"proposalId">> => ProposalId3
    })},
    
    % expect reservation extended
    ReservationId2 = test_utility:expect_reservation_succeeded("user", UserPid, ProposalId3),
    
    % expect old reservation cancelled
    test_utility:expect_reservation_cancelled("user", UserPid, ReservationId),
    
    % expect different reservation ids
    true = ReservationId2 =/= ReservationId,
    
    % wait for reservation to expire
    timer:sleep(ObjectReservationPeriod + 500),
    
    % expect reservation expired
    test_utility:expect_reservation_expired("user", UserPid, ReservationId2),
    
    % close the connection
    UserPid ! close_ws.

undo_redo_scenario(Config) ->
    % create a board
    BoardId = test_utility:post_request_to_create_board(Config),
    
    % verify board is empty
    test_utility:verify_board_is_empty(Config, BoardId),
    
    % create a user
    UserPid = test_utility:create_user("user", Config, BoardId, self(), new, undefined),
    
    % verify welcome message
    #user_state{user_id = UserId} = test_utility:expect_welcome_user("user", UserPid),
    
    % create sticky note
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
    {CanvasObjectId, _UpdatedId} = test_utility:expect_board_update_succeeded("user", UserPid, ProposalId),
    
    % reserve object for update
    ProposalId2 = utility:new_uuid(),
    UserPid ! {send, jsone:encode(#{
        <<"eventType">> => <<"reservationProposed">>,
        <<"canvasObjectId">> => CanvasObjectId,
        <<"proposalId">> => ProposalId2
    })},
    
    % expect reservation success
    ReservationId = test_utility:expect_reservation_succeeded("user", UserPid, ProposalId2),
    
    % update sticky note
    ProposalId3 = utility:new_uuid(),
    UserPid ! {send, jsone:encode(#{
        <<"eventType">> => <<"boardUpdateProposed">>,
        <<"proposalId">> => ProposalId3,
        <<"update">> => #{
            <<"canvasObjectType">> => <<"stickyNote">>,
            <<"operationType">> => <<"update">>,
            <<"canvasObjectId">> => CanvasObjectId,
            <<"operation">> => #{
                <<"canvasObjectOperationType">> => <<"updateStickyNote">>,
                <<"text">> => <<"Hello, World! Updated!">>
            }
        }
    })},
    
    % expect board update success
    {CanvasObjectId, UpdatedId2} = test_utility:expect_board_update_succeeded("user", UserPid, ProposalId3),
    
    % cancel reservation
    UserPid ! {send, jsone:encode(#{
        <<"eventType">> => <<"reservationCancellationRequested">>,
        <<"reservationId">> => ReservationId
    })},
    
    % expect reservation cancelled
    test_utility:expect_reservation_cancelled("user", UserPid, ReservationId),
    
    % undo should fail only if object is reserved for someone else
    % otherwise it should succeed unless operation is not legal (not matching the current state of the object)
    UserPid ! {send, jsone:encode(#{
        <<"eventType">> => <<"undoRequested">>
    })},
    
    % expect board update
    {<<"update">>, Operation} =
        test_utility:expect_board_updated("user", UserPid, UpdatedId2 + 1, UserId, CanvasObjectId),
    
    <<"Hello, World!">> = proplists:get_value(<<"text">>, Operation),
    
    UserPid ! {send, jsone:encode(#{
        <<"eventType">> => <<"redoRequested">>
    })},
    
    % expect board update
    {<<"update">>, Operation2} =
        test_utility:expect_board_updated("user", UserPid, UpdatedId2 + 2, UserId, CanvasObjectId),
    
    <<"Hello, World! Updated!">> = proplists:get_value(<<"text">>, Operation2),
    
    UserPid ! ws_close.

drawer(TestName, Pid, TestRunner) ->
    Delays = lists:foldl(
        fun (K, Accu) ->
            ProposalId = utility:new_uuid(),
            Msg = jsone:encode(#{
                <<"eventType">> => <<"boardUpdateProposed">>,
                <<"proposalId">> => ProposalId,
                <<"update">> => #{
                    <<"canvasObjectType">> => <<"canvas">>,
                    <<"operationType">> => <<"draw">>,
                    <<"operation">> => #{
                        <<"canvasObjectOperationType">> => <<"draw">>,
                        <<"points">> => [
                            #{
                                <<"x">> => random:uniform(800),
                                <<"y">> => random:uniform(800)
                            },
                            #{
                                <<"x">> => random:uniform(800),
                                <<"y">> => random:uniform(800)
                            }
                        ]
                    }
                }
            }),
            SentTime = test_utility:send_msg_to_user(Pid, Msg),
            {_, _, ReceivedTime} = test_utility:expect_board_update_succeeded_and_received_time(TestName, Pid, ProposalId, 200),
            Delay = ReceivedTime - SentTime,
            [Delay | Accu]
        end,
        [],
        lists:seq(1, 50)
    ),
    Pid ! {set_test_runner, TestRunner},
    TestRunner ! {drawer_delays, TestName, Delays}.

benchmark1(Config) ->
    % create a board
    BoardId = test_utility:post_request_to_create_board(Config),
    
    % verify board is empty
    test_utility:verify_board_is_empty(Config, BoardId),
    
    % create 10 users
    Users = lists:foldl(
        fun(N, Accu) ->
            [{N, test_utility:create_user(N, Config, BoardId, self(), new, undefined)} | Accu]
        end,
        [],
        lists:seq(1, 10)
    ),
    
    TestRunner = self(),
    
    % expect welcome messages
    Drawers = lists:foldl(
        fun({N, Pid}, Accu) ->
            test_utility:expect_welcome_user(N, Pid),
            NewTestRunner = spawn_link(fun () -> drawer(N, Pid, TestRunner) end),
            Pid ! {set_test_runner, NewTestRunner},
            [{N, NewTestRunner} | Accu]
        end,
        [],
        Users
    ),
    
    % expect drawer delays
    lists:foreach(
        fun({N, Pid}) ->
            receive
                {drawer_delays, N, Delays} ->
                    Avg = lists:sum(Delays) / length(Delays),
                    file:write_file("benchmark1_results", io_lib:format("~p: ~w, Avg: ~p~n", [N, Delays, Avg]), [append])
            end
        end,
        Drawers
    ),
    
    % close all connections
    lists:foreach(
        fun({_, Pid}) ->
            Pid ! close_ws
        end,
        Users
    ).

benchmark2(Config) ->
    % create a board
    BoardId = test_utility:post_request_to_create_board(Config),
    
    % verify board is empty
    test_utility:verify_board_is_empty(Config, BoardId),
    
    User1 = test_utility:create_user("user1", Config, BoardId, self(), new, undefined),
    #user_state{user_id = User1Id} = test_utility:expect_welcome_user("user1", User1),
    
    % create 30 users
    Users = lists:foldl(
        fun(N, Accu) ->
            [{N, test_utility:create_user(N, Config, BoardId, self(), new, undefined)} | Accu]
        end,
        [],
        lists:seq(1, 30)
    ),
    
    % expect welcome messages
    lists:foreach(
        fun({N, Pid}) ->
            test_utility:expect_welcome_user_with_timeout(N, Pid, 500)
        end,
        Users
    ),
    
    lists:foreach(
        fun(K) ->
            % update board
            ProposalId = utility:new_uuid(),
            SentTime = test_utility:send_msg_to_user(User1, jsone:encode(#{
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
            })),
            
            % expect board update success
            {CanvasObjectId, UpdatedId, SuccessReceivedTime} = test_utility:expect_board_update_succeeded_and_received_time("user1", User1, ProposalId, 500),
            
            file:write_file("benchmark2_results", io_lib:format("Run ~p, Success Delay: ~p~n", [K, SuccessReceivedTime - SentTime]), [append]),
            
            % expect other users to see the update
            Delays = lists:foldl(
                fun({N, Pid}, Acc) ->
                    {_, _, ReceivedTime} = test_utility:expect_board_updated_and_received_time(N, Pid, UpdatedId, User1Id, CanvasObjectId, 500),
                    Delay = ReceivedTime - SentTime,
                    [Delay | Acc]
                end,
                [],
                Users
            ),
            Avg = lists:sum(Delays) / length(Delays),
            file:write_file("benchmark2_results", io_lib:format("Run ~p: ~w, Avg: ~p~n", [K, Delays, Avg]), [append])
        end,
        lists:seq(1, 20)
    ),
    
    User1 ! close_ws,
    
    % close all connections
    lists:foreach(
        fun({_, Pid}) ->
            Pid ! close_ws
        end,
        Users
    ).