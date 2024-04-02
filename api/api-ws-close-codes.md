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
| 1000  | normal closure (when timeout or stop) by cowboy |
| 1002  | {error, badframe} by cowboy                     |
| 1006  | nginx timeout                                   |
| 1007  | {error, badencoding} by cowboy                  |
| 1009  | {error, badsize} by cowboy                      |
| 1011  | websocket handler crash by cowboy               |
