import axios from "axios"
import { v4 as uuidv4 } from "uuid"

const RESERVATION_TIMEOUT = 10 /* in seconds */

function wsPathOnsameHost(path) {
    const loc = window.location
    return (loc.protocol === "https:" ? "wss" : "ws") + "://" + loc.host + path;
}

function extractBoardIdFromPath() {
    const s = window.location.pathname.split("/")
    return s[s.length - 1]
}

export class Session {
    get _state() {
        return this._state_obj()
    }

    constructor(state, update) {
        this._state_obj = state
        this._updateState = update
        this._boardId = extractBoardIdFromPath()
        this._eventId = 1
        this._ready = false
        this._eventBuf = []
        this._sessionToken = null

        this._updateState({
            reservations: [],
            objects: [],
            users: [],
            reserved: null, // object {objectId: xxx, reservedUntil: xxx}
            user: {},
        })

        this._openWebSocketConnection()
    }

    _openWebSocketConnection() {
        // Open WebSocket Session, receive should handle here
        this._ws = new WebSocket(wsPathOnsameHost(`/api/ws/boards/${this._boardId}`))

        this._ws.onmessage = event => {
            const eventData = JSON.parse(event.data)

            console.log("Got message: ", eventData)

            // TODO Process message
            if (this._ready) {
                this._updateState(
                    this._processEvent(this._state, eventData)
                )
                console.log("state updated:", this._state)
            } else if (eventData.eventType === "welcomeUser") {
                // Save session information (e.g. ID & token)
                this._userId = eventData.userId
                this._sessionToken = eventData.sessionToken
                this._fetchInitialState()
            } else {
                this._eventBuf.push(eventData)
            }
        }

        this._ws.onopen = () => {
            if (this._sessionToken == null) {
                this.sendEvent({
                    eventType: "begin",
                    sessionType: "new"
                })
            } else {
                this.sendEvent({
                    eventType: "begin",
                    sessionType: "continue",
                    sessionToken: this._sessionToken
                })
            }
        }

        this._ws.onclose = (event) => {
            this._ready = false
            this._eventBuf = []
            this._openWebSocketConnection()
            console.log("WebSocket connection closed", event)
        }

        this._ws.onerror = (event) => {
            console.error("WebSocket error: ", event)
        }
    }

    _processEvent(state, eventData) {
        // Process events based on event type discriminator field
        switch(eventData.eventType) {
            case "reservationCancelled":
                return {
                    ...state,
                    reservations: this._state.reservations.filter(r => r.reservationId !== eventData.reservationId)
                }

            case "reservationExpired":
                return {
                    ...state,
                    reservations: this._state.reservations.filter(r => r.reservationId !== eventData.reservationId)
                }
            
            case "canvasObjectReserved":
                return {
                    ...state,
                    reservations: [...this._state.reservations, eventData]
                }

            case "userJoined":
                return {
                    ...state,
                    users: [...this._state.users, eventData]
                }

            case "userLeft":
                return {
                    ...state,
                    users: this._state.users.filter(u => u.userId !== eventData.userId)
                }

            case "boardUpdateSucceeded":
                if (eventData.update.operationType === "delete") {
                    return {
                        ...state,
                        objects: [...this._state.objects.filter(o => o.canvasObjectId !== eventData.update.canvasObjectId)],
                        reservations: [...this._state.reservations.filter(r => r.objectId !== eventData.update.canvasObjectId)]
                    }
                } else {
                    return {
                        ...state,
                        objects: [...this._state.objects.filter(o => o.canvasObjectId !== eventData.update.canvasObjectId), eventData.update]
                    }
                }

            case "boardUpdateFailed":
                alert("Committing board action failed.")
                break;

            case "boardUpdated":
                // Check incremental messge counter
                if (this._lastAppliedUpdate >= eventData.updateId) {
                    return
                }

                this._lastAppliedUpdate = eventData.updateId

                if (eventData.update.operationType === "delete") {
                    return {
                        ...state,
                        objects: [...this._state.objects.filter(o => o.canvasObjectId !== eventData.update.canvasObjectId)],
                        reservations: [...this._state.reservations.filter(r => r.objectId !== eventData.update.canvasObjectId)]
                    }
                } else {
                    return {
                        ...state,
                        objects: [...this._state.objects.filter(o => o.canvasObjectId !== eventData.update.canvasObjectId), eventData.update]
                    }
                }

            case "reservationProposalSucceeded":
                const reservation = this._state.reservations.find(r => r.proposalId === eventData.proposalId)

                if (!reservation) {
                    break;
                }

                setTimeout(
                    () => {
                        /* Renew reservation */
                        if (this._state.reservations.find(r => r.objectId === reservation.objectId)) {
                            this._proposeReservation(reservation.objectId)
                        }
                    },
                    Math.floor(0.8 * RESERVATION_TIMEOUT * 1000)
                )

                return({
                    ...this._state,
                    reservations: [
                        ...this._state.reservations.filter(r => r.proposalId !== eventData.proposalId),
                        { ...reservation, ...eventData }]
                })

            case "reservationProposalFailed":
                alert("Failed to reserve object")
                return {
                    ...this._state,
                    reservations: [...this._state.reservations.filter(r => r.proposalId !== eventData.proposalId)]
                }
                
            default:
                console.error("Unknown eventType ", eventData.eventType)
        }

        return state
    }

    getBlobResourceUrl(blobId) {
        return `/api/rest/boards/${this._boardId}/blobs/${blobId}`
    }

    sendEvent(payload) {
        console.log("send message: ", payload)
        if (this._ws.readyState === WebSocket.OPEN) {
            this._ws.send(JSON.stringify(payload));
        } else {
            console.error('WebSocket connection is not open when sending message.');
        }
    }

    proposeUpdate(update) {
        this.sendEvent({
            eventType: "boardUpdateProposed",
            proposalId: uuidv4(),
            intermediate: false,
            update,
        })
    }

    sendBlob(blob) { // Returns a promise of blobID
        return new Promise((resolve, reject) => {
            const req = new XMLHttpRequest();
            req.open("POST", `/api/rest/boards/${this._boardId}/blobs`, true);

            req.onreadystatechange = () => {
                if (req.readyState === XMLHttpRequest.DONE) {
                    const json = JSON.parse(req.responseText)
                    console.log(json)
                    resolve(json.blobId)
                }
            }

            req.send(blob);
        })
    }

    _fetchInitialState() {
        // Fetch Board State
        axios.get(`/api/rest/boards/${this._boardId}`, {
            headers: {
                'session-token': this._sessionToken
            }
        }).then((res) => {
            // TODO: initialize local state with received objects
            const data = res.data

            // TODO: Initialize last applied update
            this._lastAppliedUpdate = data.lastAppliedUpdate

            this._updateState(
                this._eventBuf.reduce(
                    (state, o) => this._processEvent(state, o),
                    {
                        ...this._state,
                        user: data.onlineUsers.filter(u => u.id === this._userId)[0],
                        objects: data.canvasObjects
                    }
                )
            )
            
            this._ready = true
        })
        .catch(err => {
            console.error(err)
        })
    }

    get sessionID() {
        return this._userId
    }

    _proposeReservation(objectId) {
        const proposalId = uuidv4()

        this._updateState({
            ...this._state,
            reservations: [...this._state.reservations.filter(r => r.objectId !== objectId), { ...this._state.reservations.find(r => r.objectId === objectId), objectId, proposalId }]
        })

        this.sendEvent({
            eventType: "reservationProposed",
            canvasObjectId: objectId,
            proposalId,
        })
    }

    reserveObject(objectId, objectType) {
        if (this._state.reservations.find(r => r.objectId === objectId)) {
            // Already reserved
            return
        }

        this._proposeReservation(objectId)


        return {
            objectId,
            cancel: () => {
                /* Cancel hook */
                console.log("Cancelling reservation")

                const reservation = this._state.reservations.find(r => r.objectId === objectId)
                if (!reservation) {
                    return
                }

                this.sendEvent({
                    eventType: "reservationCancellationRequested",
                    reservationId: reservation.reservationId,
                })

                // this._updateState({
                //     ...this._state,
                //     reservations: [...this._state.reservations.filter(r => r.objectId !== objectId)]
                // })
            },
            type: objectType
        }
    }
}