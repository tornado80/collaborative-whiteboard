import threading
import websocket
import json
import time
import uuid
import requests
import pytest

WEBSOCKET_URL = "ws://localhost:8080/api/ws/boards/"
REST_URL = "http://localhost:8080/api/rest/boards"

response = requests.post(REST_URL)
assert response.status_code == 201

# Extract boardId from the response headers
board_id = response.headers["Location"].split("/")[-1]

def begin():

    #begin
    ws = websocket.create_connection(WEBSOCKET_URL + board_id)
    payload = {
        "eventType": "begin",
        "sessionToken": uuid.uuid1(), 
        "sessionType": "new" 
    }
    ws.send(json.dumps(payload))
    response = ws.recv()
    assert '{"eventType": "welcomeUser"}' in response
    ws.close()

def send_update(payload):
    ws = websocket.create_connection(WEBSOCKET_URL + board_id)
    start_time = time.time()  # Record the start time
    ws.send(json.dumps(payload))
    end_time = time.time()  # Record the end time after sending
    response = ws.recv()
    print("Client received:", response)
    ws.close()
    return end_time - start_time  # Return the time taken for sending

def simulate_clients():
    # Client 1 sends boardUpdateProposedPayload
   
    update_payload = {
        "eventType": "boardUpdateProposed",
        "proposalId": uuid.uuid1(),
        "update": { 
            "canvasObjectId": uuid.uuid1(),
            "canvasObjectType": 'stickyNote',
            "operationType": 'create',
            "operation": {
                
                "canvasObjectOperationType": 'createStickyNote',
                "position": {
                    "x": 100,
                    "y": 30
                },

                "text": 'Hello world',
                "color": '#00000000'

            }
         }  
    }
    send_time = send_update(update_payload)
    print("Time taken for Client 1 to send update:", send_time)

    # Clients 2-10 receive broadcast of boardUpdated
    receive_times = []
    for i in range(2, 11):
        receive_times.append(receive_update())
    
    avg_receive_time = sum(receive_times) / len(receive_times)
    print("Average time taken for Clients 2-10 to receive update:", avg_receive_time)

def receive_update():
    ws = websocket.create_connection(WEBSOCKET_URL + board_id)
    start_time = time.time()  # Record the start time
    response = ws.recv()
    end_time = time.time()  # Record the end time after receiving
    print("Client received:", response)
    ws.close()
    return end_time - start_time  # Return the time taken for receiving

if __name__ == "__main__":
    begin()
    simulate_clients()





