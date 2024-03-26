-include("common_records.hrl").

-record(comment, {
    %id :: binary(),
    text :: binary(),
    timestamp :: integer(),
    imageId :: binary()
}).

-record(image, {
    %id :: binary(),
    zIndex :: integer(),
    position :: #vector2{}, % [{<<"x">>, X}, {<<"y">>, Y}]
    color :: binary(),
    width :: integer(),
    size :: #vector2{}, % [{<<"x">>, X}, {<<"y">>, Y}]
    blobId :: binary()
}).

-record(drawing_curve, {
    %id :: binary(),
    points :: [#vector2{}], % [[{<<"x">>, X}, {<<"y">>, Y}]]
    color :: binary()
}).

-record(erasing_curve, {
    %id :: binary(),
    centers :: [#vector2{}], % [[{<<"x">>, X}, {<<"y">>, Y}]]
    radius :: integer()
}).

-record(stickyNote, {
    %id :: binary(),
    zIndex :: integer(),
    position :: #vector2{}, % [{<<"x">>, X}, {<<"y">>, Y}]
    color :: binary(),
    text :: binary()
}).

-record(update, {
    id :: integer(),
    objectId :: binary(),
    objectType :: image | drawingCurve | stickyNote | comment | erasingCurve,
    operationType :: create | update | delete,
    oldValue :: undefined | #comment{} | #image{} | #drawing_curve{} | #erasing_curve{} | #stickyNote{},
    newValue :: undefined | #comment{} | #image{} | #drawing_curve{} | #erasing_curve{} | #stickyNote{}
}).

-record(session, {
    userId :: binary(),
    userName :: binary(),
    color :: binary(),
    wsPid :: pid(),
    status :: online | offline,
    sessionToken :: binary(),
    undoStack :: [#update{}],
    redoStack :: [#update{}],
    inactivityTimerRef :: reference()
}).

-record(board, {
    id :: binary(),
    name :: binary(),
    sessions :: [#session{}],
    objects :: [#image{} | #drawing_curve{} | #stickyNote{} | #comment{} | #erasing_curve{}],
    lastUpdateId :: integer()
}).