-module(rest_handlers_tests).

-include_lib("eunit/include/eunit.hrl").


%{ok, Pid} = gun:open("localhost", 8080),
 %   Ref = gun:ws_upgrade(Pid, "/api/ws"),
    
  %  gun:ws_send(Pid, Ref, <<"Hello">>),
   % gun:ws_send(Pid, Ref, <<"World">>).

create_board_handler_test() ->
    % Arrange
    {ok, _} = application:ensure_all_started(backend),
    {ok, _} = application:ensure_all_started(gun),

    % Act
    {ok, Pid} = gun:open("localhost", 8080, #{protocols => [http2]}),
    ?assertEqual(http2, maps:get(protocol, gun:info(Pid))),
    StreamRef = gun:post(Pid, "/api/rest/boards", []),
    Response = gun:await(Pid, StreamRef),

    % Assert
    ?assertMatch({response, nofin, 201, _Headers}, Response),
    {response, nofin, 201, Headers} = Response,
    LocationHeader = lists:keyfind(<<"location">>, 1, Headers),
    {<<"location">>, <<"/boards/", BoardId/binary>>} = LocationHeader,

    % Act
    ResponseBody = gun:await_body(Pid, StreamRef),

    % Assert
    ?assertMatch({ok, _Body}, ResponseBody),
    {ok, Body} = ResponseBody,
    ?assertEqual([{<<"boardId">>, BoardId}], jsone:decode(Body, [{object_format, proplist}])),

    % Cleanup
    gun:close(Pid),
    application:stop(backend),
    application:stop(gun).
