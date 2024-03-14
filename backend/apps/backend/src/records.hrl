-record(get_board_handler_state, {
    boardId :: binary(),
    sessionToken :: binary(),
    boardManagerPid :: pid()
}).

-record(blob_handler_state, {
    sessionToken :: binary(),
    boardCacheServicePid :: pid(),
    blobId :: binary(),
    blob :: binary()
}).

-record(websocket_handler_state, {
    sessionToken = binary(),
    boardId :: binary(),
    boardManagerPid :: pid()
}).

-record(user, {
    id :: binary(),
    name :: binary(),
    color :: binary()
}).

-record(vector2, {
    x :: integer(),
    y :: integer()
}).

-record(comment, {
    id :: binary(),
    text :: binary(),
    timestamp :: integer()
}).

-record(image, {
    id :: binary(),
    zindex :: integer(),
    position :: #vector2{},
    color :: binary(),
    width :: integer(),
    size :: #vector2{},
    comments :: [#comment{}],
    blobId :: binary(),
    canvasObjectType = <<"Image">>
}).

-record(curve, {
    points :: [#vector2{}],
    color :: binary(),
    canvasObjectType = <<"Curve">>    
}).

-record(stickyNote, {
    id :: binary(),
    zindex :: integer(),
    position :: #vector2{},
    color :: binary(),
    text :: binary(),
    canvasObjectType = <<"StickyNote">>
}).

-record(board, {
    id :: binary(),
    name :: binary(),
    lastChangeId :: integer(),
    activeUsers :: [#user{}],
    canvasObjects :: [#stickyNote{} | #curve{} | #image{}]
}).