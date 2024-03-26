

class StickyNote {
    constructor(json) {
        const o = JSON.parse(json)
    }

    toJSON() {
        return JSON.stringify({
            type: ""
        })
    }
}

const EventType = {
    Begin: "begin",
    ReservationProposed: "reservationProposed",
    ReservationProposalSucceeded: "reservationProposalSucceeded",
    ReservationProposalFailed: "reservationProposalFailed",
    ReservationCancellationRequested: "reservationCancellationRequested",
    ReservationCancelled: "reservationCancelled",
    ReservationExpired: "reservationExpired",
    CanvasObjectReserved: "canvasObjectReserved",
    UserJoined: "userJoined",
    UserLeft: "userLeft",
    WelcomeUser: "welcomeUser",
    BoardUpdated: "boardUpdated",
    BoardUpdateProposed: "boardUpdateProposed",
    BoardUpdateFailed: "boardUpdateFailed",
    BoardUpdateSucceeded: "boardUpdateSucceeded",
    UndoRequested: "undoRequested",
    RedoRequested: "redoRequested"
}
