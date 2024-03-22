-module(rest_handlers_tests).

-include_lib("eunit/include/eunit.hrl").

websocket_handler_test() ->
    % Arrange
    application:ensure_all_started(backend),
    application:ensure_all_started(gun),

    % Act
    {ok, Pid} = gun:open("localhost", 8080),
    StreamRef = gun:ws_upgrade(Pid, "/api/ws/boards/"),
    {ok, Message} = gun:await(Pid, StreamRef),

    % Assert
    ?assertMatch({ws_upgrade, {response, nofin, 101, _Headers}}, Message),

    % Cleanup
    gun:close(Pid),
    application:stop(backend),
    application:stop(gun).