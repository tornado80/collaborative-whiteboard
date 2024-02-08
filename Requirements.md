# Functional Requirements
1. Users' actions should not interfere with each other and cause the board to freeze or become unusable or unreliable for current and other users

Actions could be the following:
- Joining or leaving session
- crashing client or unexpected disconnection or network failure
- Uploading or donloading images
- Simultanuous drawing

Once a user selects an object, the object is reserved for them for a specfic fixed time, which can be extended. Once the object is reserved, only that user can make modifications to it. This applies to all objects except the drawing pen or eraser.

2. At least 2 users should be able to join the session.

# Non-functional Requirements

## Sharing the board
Sharing the board should be easy.

## Security
1. Joining the session should not be possible without knowing the session id
2. Communication is encrypted

## Scalability
## Latency
user should not feel a lag when drawing or viewing other users' actions

# Architecture
