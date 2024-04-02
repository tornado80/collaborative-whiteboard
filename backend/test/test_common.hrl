-record(user_state, {
    test_name,
    server_name = undefined,
    test_runner,
    conn_pid,
    stream_ref,
    board_id,
    user_id,
    session_token,
    monitor_ref
}).