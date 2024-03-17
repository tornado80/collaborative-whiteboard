-module(board_controller_service).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions
-export([
    get_board_state/1,
    new_session/1,
    continue_session/3,
    send_event/3
]).

%% State record
-record(state, {}).

%% API functions
get_board_state(Pid) ->
    gen_server:call(Pid, get_board_state).

new_session(Pid) ->
    gen_server:call(Pid, {new_session, self()}).

continue_session(Pid, SessionToken, LastEventId) ->
    gen_server:call(Pid, {continue_session, self(), SessionToken, LastEventId}).

send_event(Pid, SessionToken, Event) ->
    gen_server:call(Pid, {send_event, SessionToken, Event}).

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