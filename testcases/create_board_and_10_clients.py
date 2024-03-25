import threading
import websocket
import json
import time

WEBSOCKET_URL = "ws://localhost:8080/api/ws/boards/3a79cd00-e5be-40c5-b4"

def test_whiteboard_creation():
    ws = websocket.create_connection(WEBSOCKET_URL)
    event = {
        "eventType": "begin",
        "eventPayload": {
            "sessionToken": "", #here
            "sessionType": "" #here
        }
    }
    ws.send(json.dumps(event))
    response = ws.recv()
    assert response == '{"eventType": "welcomeUser"}'
    ws.close()

def send_update(payload):
    ws = websocket.create_connection(WEBSOCKET_URL)
    start_time = time.time()  # Record the start time
    ws.send(json.dumps(payload))
    end_time = time.time()  # Record the end time after sending
    response = ws.recv()
    print("Client received:", response)
    ws.close()
    return end_time - start_time  # Return the time taken for sending

def simulate_clients():
    # Client 1 sends boardUpdateProposedPayload
    proposal_id = "" #here
    update_payload = {
        "eventType": "boardUpdateProposed",
        "proposalId": proposal_id,
        "update": {  }  # Fill in with your update payload #here
    }
    send_time = send_update(update_payload)
    print("Time taken for Client 1 to send update:", send_time)

    # Clients 2-10 receive boardUpdatedPayload or boardUpdateFailedPayload
    receive_times = []
    for i in range(2, 11):
        receive_times.append(receive_update())
    
    avg_receive_time = sum(receive_times) / len(receive_times)
    print("Average time taken for Clients 2-10 to receive update:", avg_receive_time)

def receive_update():
    ws = websocket.create_connection(WEBSOCKET_URL)
    start_time = time.time()  # Record the start time
    response = ws.recv()
    end_time = time.time()  # Record the end time after receiving
    print("Client received:", response)
    ws.close()
    return end_time - start_time  # Return the time taken for receiving

if __name__ == "__main__":
    simulate_clients()





