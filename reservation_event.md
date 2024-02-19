1. Propose the reservation with following attributes:
    request: object ID, 

>Client to Server

2. Result of reserve propose(positive result)
    response:  object ID, user ID, reserved_until: timestamp
>Broadcast

3. Result of reserve propose(negative result)
    response: objectID
> Server to Client

4. Client Reservation
    request: objectID

> CLient to Server

