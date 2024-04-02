### Our application close codes
| Code  | Reason                                          |
|-------|-------------------------------------------------|
| 4000  | board controller is down                        |
| 4001  | this endpoint does not accept binary stream     |
| 4002  | session token is not provided                   |
| 4003  | malformed begin event                           |
| 4004  | invalid session type                            |
| 4005  | invalid json                                    |
| 4006  | eventType is not provided                       |
| 4007  | unexpected eventType                            |

### RFC 6455 (Nginx, and Cowboy close codes implementations)
| Code  | Reason                                          | Who?   |
|-------|-------------------------------------------------|--------|
| 1000  | normal closure (when timeout or stop) by cowboy | cowboy |
| 1002  | {error, badframe} by cowboy                     | cowboy |
| 1006  | nginx timeout                                   | nginx  |
| 1007  | {error, badencoding} by cowboy                  | cowboy |
| 1009  | {error, badsize} by cowboy                      | cowboy |
| 1011  | websocket handler crash by cowboy               | cowboy |

More information about the close codes can be found 
in the [RFC 6455](https://tools.ietf.org/html/rfc6455#section-7.4.1) and `websocket_send_close/2` function in 
[Cowboy's websocket module source code](https://github.com/ninenines/cowboy/blob/8cb9d242b0a665cada6de8b9a9dfa329e0c06ee9/src/cowboy_websocket.erl#L661C1-L661C21).