-record(get_board_handler_state, {
    boardId :: binary(),
    sessionToken :: binary(),
    boardControllerPid :: pid()
}).

-record(blob_handler_state, {
    boardId :: binary(),
    blobId :: binary(),
    blob :: binary(),
    boardCacheServicePid :: binary()
}).

-record(websocket_handler_state, {
    sessionRef :: reference(),
    sessionToken :: binary(),
    boardId :: binary(),
    boardControllerPid :: pid()
}).