-include("common_records.hrl").

-record(comment, {
    id :: binary(),
    text :: binary(),
    timestamp :: integer(),
    imageId :: binary(),
    reservedBy :: reference()
}).

-record(image, {
    id :: binary(),
    zIndex :: integer(),
    position :: #vector2{},
    color :: binary(),
    width :: integer(),
    size :: #vector2{},
    blobId :: binary(),
    reservedBy :: reference()
}).

-record(drawing_curve, {
    id :: binary(),
    zIndex :: integer(),
    points :: [#vector2{}],
    color :: binary()
}).

-record(erasing_curve, {
    centers :: [#vector2{}],
    size :: integer()
}).

-record(stickyNote, {
    id :: binary(),
    zIndex :: integer(),
    position :: #vector2{},
    color :: binary(),
    text :: binary(),
    reservedBy :: reference()
}).

-record(update, {
    id :: integer(),
    objectId :: binary(),
    objectType :: image | curve | stickyNote | comment,
    operationType :: create | update | delete,
    operation ::
        createImage | createCurve | createStickyNote | createComment |
        updateImage | updateStickyNote | updateComment |
        deleteImage | deleteCurve | deleteStickyNote | deleteComment,
    oldState :: undefined | #comment{} | #image{} | #drawing_curve{} | #erasing_curve{} | #stickyNote{},
    newState :: undefined | #comment{} | #image{} | #drawing_curve{} | #erasing_curve{} | #stickyNote{}
}).

-record(session, {
    userId :: binary(),
    userName :: binary(),
    color :: binary(),
    wsPid :: pid(),
    status :: online | offline,
    sessionToken :: binary(),
    undoStack :: [#update{}],
    redoStack :: [#update{}]
}).

-record(board, {
    id :: binary(),
    name :: binary(),
    sessions :: [#session{}],
    objects :: [#image{} | #drawing_curve{} | #stickyNote{} | #comment{}],
    lastUpdateId :: integer()
}).