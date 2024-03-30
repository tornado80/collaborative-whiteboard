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

-record(board_state,{
    test_name,
    supervisor,
    board_id,
    user_id,
    session_token    
}).

- record (object_state,{
    test_name,
    board_id,
    supervisor,
    session_token,
    object_id,
    user_id,
    }).