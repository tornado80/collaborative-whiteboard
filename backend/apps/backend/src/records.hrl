-record(event, {
    eventType :: atom(),
    eventId :: integer(),
    eventContent
}).

-record(begin_event, {
    sessionType :: atom(),
    sessionToken :: binary(),
    lastEventId :: integer()
}).

-record(reservation_propose_event, {
    canvasObjectId :: binary(),
    proposalId :: binary()
}).

-record(reservation_cancel_event, {
    reservationId :: binary()
}).

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
    sessionToken :: binary(),
    boardId :: binary(),
    boardControllerPid :: pid()
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

-record(board_update, {
    proposalId :: binary(),
    update :: binary(),
    intermediate :: atom(),
    canvasObjectId :: binary()
}).