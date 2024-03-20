What should we do in case of websocket connection failure?

We have to resend the client some events. 
- Which events? 
  - missed board updates 
  - previous reservations

Client should also reconnect to the server and request the events that it missed.
- Through which protocol? 
  - Websocket
    - begin message
  - REST API