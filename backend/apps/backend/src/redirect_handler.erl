-module(redirect_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, BoardId} = boards_manager_service:create_board(),
    Uri = <<"/boards/", BoardId/binary>>,
    Req1 = cowboy_req:set_resp_body(<<"Redirecting to ", Uri/binary>>, Req0),
    Req2 = cowboy_req:set_resp_header(<<"location">>, Uri, Req1),
    Req3 = cowboy_req:reply(303, Req2),
    {ok, Req3, State}.
