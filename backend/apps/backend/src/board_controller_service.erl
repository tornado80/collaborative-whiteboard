-module(board_controller_service).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions
-export([
    get_board_state/1,
    new_session/1,
    continue_session/2,
    reserve_canvas_object/3,
    cancel_reservation/3,
    update_board/3,
    undo/2,
    redo/2
]).

%% State record
-record(state, {}).

%% API functions

-spec get_board_state(Pid :: pid()) -> binary().
get_board_state(Pid) ->
    gen_server:call(Pid, get_board_state).

-spec new_session(Pid :: pid()) ->
    {ok, binary(), binary()}.
new_session(Pid) ->
    gen_server:call(Pid, {new_session, self()}).

-spec continue_session(Pid :: pid(), SessionToken :: binary()) ->
    {ok, binary(), binary()}.
continue_session(Pid, SessionToken) ->
    gen_server:call(Pid, {continue_session, self(), SessionToken}).

-spec reserve_canvas_object(Pid :: pid(), CanvasObjectId :: binary(), SessionToken :: binary()) ->
    {ok, ReservationId :: binary(), ExpirationTimeStamp :: integer()} | {error, binary()}.
reserve_canvas_object(Pid, CanvasObjectId, SessionToken) ->
    gen_server:call(Pid, {reserve_canvas_object, CanvasObjectId, SessionToken}).

% already_reserved -> <<"This object is already reserved.">>;
% object_not_found -> <<"This object is not found.">>;
% session_not_found -> <<"This session does not exist.">>;
% board_not_found -> <<"The board does not exist.">>;
% _ -> <<"Unexpected behaviour.">>

cancel_reservation(Pid, ReservationId, SessionToken) ->
    gen_server:cast(Pid, {cancel_reservation, ReservationId, SessionToken}).

undo(Pid, SessionToken) ->
    gen_server:cast(Pid, {undo, SessionToken}).

redo(Pid, SessionToken) ->
    gen_server:cast(Pid, {redo, SessionToken}).

-spec update_board(Pid :: pid(), Update :: binary(), SessionToken :: binary()) ->
    ok | {error, binary()}.
update_board(Pid, Update, SessionToken) ->
    gen_server:call(Pid, {update_board, Update, SessionToken}).

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