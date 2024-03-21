import axios from "axios"
import { Reservation } from "./reservation";

function wsPathOnsameHost(path) {
    const loc = window.location
    return (loc.protocol === "https:" ? "wss" : "ws") + "://" + loc.host + path;
}

function extractBoardIdFromPath() {
    const match = window.location.pathname.match(/\/boards\/(\d+)/)
    if (match) {
        return parseInt(match[1])
    }
    return null
}

export class Session {
    constructor(state, update) {
        this._state = state
        this._updateState = update
        this._boardId = extractBoardIdFromPath()
        this._eventId = 1
        this._ready = false
        this._mutationEventBuf = []

        // Open WebSocket Session, receive should handle here
        this._ws = new WebSocket(wsPathOnsameHost(`/api/ws/boards/${this._boardId}`))

        this._ws.onmessage = event => {
            const eventData = JSON.parse(event.data)

            switch(eventData.eventType) {
                // TODO: Process events based on event type discriminator field
                case "welcomeUser":
                    // TODO: Save session information (e.g. ID & token)
                    this._fetchInitialState()
                    break;
            }
        }

        this._ws.onopen = () => {
        }
        this._sessionId = 123
        this._sessionToken = "asddfqwerty123"
    }

    _processStateMutationEvent(updateEvent) {
        // Ignore past updates
        if (this._lastAppliedUpdate >= updateEvent.updateId) {
            return
        }

        switch(updateEvent.objectType) {
            // TODO: Process events based on event type discriminator field
            case "stickyNote":
                this._updateObject(updateEvent.object.id, {
                    id: updateEvent.object.id,
                    pos: updateEvent.object.pos,
                    // etc.
                })
                break;
        }
    }

    _fetchInitialState() {
        // Fetch Board State
        axios.get(`/api/rest/boards/${this._boardId}`, {
            headers: {
                'X-Session-Token': this._sessionToken
            }
        }).then((res) => {
            // TODO: initialize local state with received objects
            const data = res.data

            // TODO: Initialize last applied update
            this._lastAppliedUpdate = data.lastAppliedUpdate

            // TODO: Process cached updates
            this._mutationEventBuf.forEach(this._processStateMutationEvent)
        })
    }

    _updateObject(objectId, objectData) {
        this._updateState(this._state.map(o => (o.id === objectId) ? objectData : o).sort((a, b) => a.z_index - b.z_index))
    }

    _insertObject(objectData) {
        this._updateState([...this._state, objectData].sort((a, b) => a.z_index - b.z_index))
    }

    _removeObject(objectId) {
        this._updateState(this._state.filter(o => o.id !== objectId))
    }

    get sessionID() {
        return this._sessionId
    }

    reserveObject(objectId) {
        const r = new Reservation(this._sessionToken, objectId)
    }
}