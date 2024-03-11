-module(websocket_handler_tests).

-include_lib("eunit/include/eunit.hrl").

simple_message_test() ->
    {ok, Pid} = gun:open("localhost", 8080),
    Ref = gun:ws_upgrade(Pid, "/api/ws"),
    gun:ws_send(Pid, Ref, <<"Hello">>),
    gun:ws_send(Pid, Ref, <<"World">>).