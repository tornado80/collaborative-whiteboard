import axios from "axios"
import { Reservation } from "./reservation"
import { v4 as uuidv4 } from "uuid"

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

        this._updateState({
            reservations: [],
            objects: [],
            users: [],
            reserved: null, // object {objectId: xxx, reservedUntil: xxx}
            user: {},
        })

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
            this.sendEvent({
                eventType: "begin",
                sessionType: "new"
            })
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
                return {
                    ...state,
                    objects: [...this._state.objects.filter(o => o?.canvasObjectId !== eventData.update.canvasObjectId), eventData.update]
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

                return {
                    ...state,
                    objects: [...this._state.objects.filter(o => o.canvasObjectId !== eventData.update.canvasObjectId), eventData.update]
                }

            case "reservationProposalSucceeded":
                break

            case "reservationProposalFailed":
                alert("Failed to reserve object")
                break

            case "boardUpdateFailed":
                alert("Failed to update object")
                break
                
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
            console.error('WebSocket connection is not open.');
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
                if (req.readyState == XMLHttpRequest.DONE) {
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
        console.log("board", `/api/rest/boards/${this._boardId}`)
        axios.get(`/api/rest/boards/${this._boardId}`, {
            headers: {
                'session-token': this._sessionToken
            }
        }).then((res) => {
            console.log("Got initial state: ", res)

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

    reserveObject(objectId) {
        //const r = new Reservation(this._sessionToken, objectId)
    }
}