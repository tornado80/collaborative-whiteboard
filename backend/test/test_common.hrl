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