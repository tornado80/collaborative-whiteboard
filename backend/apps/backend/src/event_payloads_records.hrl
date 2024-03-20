-include("common-records.hrl").

-record(begin_payload, {
    sessionType :: atom(),
    sessionToken :: binary(),
    lastEventId :: integer()
}).

-record(welcome_user_payload, {
    userId :: binary(),
    sessionToken :: binary()
}).

-record(reservation_proposed_payload, {
    canvasObjectId :: binary(),
    proposalId :: binary()
}).

-record(reservation_proposal_succeeded_payload, {
    proposalId :: binary(),
    reservationId :: binary(),
    expirationTimestamp :: integer()
}).

-record(reservation_proposal_failed_payload, {
    proposalId :: binary(),
    errorMessage :: binary()
}).

-record(reservation_cancellation_requested_payload, {
    reservationId :: binary()
}).

-record(reservation_cancelled_payload, {
    reservationId :: binary()
}).

-record(reservation_expired_payload, {
    reservationId :: binary()
}).

-record(canvas_object_reserved_payload, {
    canvasObjectId :: binary(),
    reservationId :: binary(),
    userId :: binary(),
    expirationTimestamp :: integer()
}).

-record(user_payload, {
    userId :: binary()
}).

-record(create_sticky_note_payload, {
    text :: binary(),
    position :: {integer(), integer()},
    color :: binary()
}).

-record(update_sticky_note_payload, {
    canvasObjectId :: binary(),
    text :: binary(),
    position :: #vector2{},
    color :: binary()
}).

-record(delete_sticky_note_payload, {
    canvasObjectId :: binary()
}).

-record(create_comment_payload, {
    text :: binary(),
    timestamp :: integer()
}).

-record(update_comment_payload, {
    canvasObjectId :: binary(),
    text :: binary(),
    timestamp :: integer()
}).

-record(delete_comment_payload, {
    canvasObjectId :: binary()
}).

-record(update_image_payload, {
    canvasObjectId :: binary(),
    name :: binary(),
    position :: #vector2{},
    width :: integer()
}).

-record(delete_image_payload, {
    canvasObjectId :: binary()
}).

-record(draw_payload, {
    points :: [#vector2{}],
    color :: binary()
}).

-record(erase_payload, {
    centers :: [#vector2{}],
    size :: integer()
}).

-record(create_image_payload, {
    name :: binary(),
    position :: #vector2{},
    width :: integer(),
    size :: #vector2{}
}).

-record(canvas_object_operation, {
    canvasObjectOperationType :: atom(),
    canvasObjectOperationContent ::
        #create_sticky_note_payload{}
}).

-record(update_payload, {
    canvasObjectId :: binary(),
    canvasObjectType :: atom(),
    operation :: atom(),
    newValue :: #canvas_object_operation{}
}).

-record(board_update_proposed_payload, {
    proposalId :: binary(),
    update :: #update_payload{},
    intermediate :: boolean()
}).

-record(board_update_succeeded_payload, {
    proposalId :: binary(),
    updateContent :: #update_payload{}
}).

-record(board_update_failed_payload, {
    proposalId :: binary(),
    errorMessage :: binary()
}).

-record(board_updated_payload, {
    changeId :: integer(),
    userId :: binary(),
    intermediate :: boolean(),
    updateContent :: #update_payload{}
}).

-record(event, {
    eventType :: atom(),
    eventId :: integer(),
    eventContent ::
    #begin_payload{} | % client to server
    #reservation_proposed_payload{} | % client to server
    #reservation_cancellation_requested_payload{} | % client to server
    #board_update_proposed_payload{} | % client to server
    #board_update_failed_payload{} | % server reply to client
    #board_update_succeeded_payload{} | % server reply to client
    #board_updated_payload{} | % server broadcast
    #welcome_user_payload{} | % server reply to client
    #user_payload{} | % server broadcast
    #reservation_proposal_succeeded_payload{} | % server reply to client
    #reservation_proposal_failed_payload{} | % server reply to client
    #reservation_cancelled_payload{} | % server broadcast
    #reservation_expired_payload{} | % server broadcast
    #canvas_object_reserved_payload{} % server broadcast
}).