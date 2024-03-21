-include("common-records.hrl").

-record(user, {
    id :: binary(),
    name :: binary(),
    color :: binary()
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
    zIndex :: integer(),
    position :: #vector2{},
    color :: binary(),
    text :: binary(),
    canvasObjectType = <<"StickyNote">>
}).

-record(board, {
    id :: binary(),
    name :: binary(),
    lastUpdateId :: integer(),
    activeUsers :: [#user{}],
    canvasObjects :: [#stickyNote{} | #curve{} | #image{}]
}).