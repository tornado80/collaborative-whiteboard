-module(board_controller_service).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions
-export([get_board_state/1]).

%% State record
-record(state, {}).

%% API functions
get_board_state(Pid) ->
    gen_server:call(Pid, get_board_state).

create_session_token(Pid) ->
    gen_server:call(Pid, create_session_token).

subscribe_to_board(Pid, SessionToken, SubscriberPid) ->
    gen_server:call(Pid, {subscribe_to_board, SessionToken, SubscriberPid}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% Callback functions
init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.