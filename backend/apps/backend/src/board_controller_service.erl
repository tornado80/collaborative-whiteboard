-module(board_controller_service).
-behaviour(gen_server).

-include("board_state_records.hrl").
-include("event_payloads_records.hrl").

-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions
-export([
    get_board_state/1,
    new_session/1,
    continue_session/2,
    reserve_canvas_object/3,
    cancel_reservation/3,
    update_board/3,
    undo/2,
    redo/2,
    end_session/3
]).

%% State record
-record(state, {
    board_id :: binary(),
    name :: binary(),
    board_sessions_table :: ets:tid(),
    board_session_tokens_table :: ets:tid(),
    board_objects_table :: ets:tid(),
    last_update_id :: integer(),
    supervisor_pid :: pid()
}).

%% API functions

-spec get_board_state(Pid :: pid()) -> Json :: binary().
get_board_state(Pid) ->
    Board = gen_server:call(Pid, get_board_state),
    jsone:encode(
        #{
            id => Board#board.id,
            lastUpdateId => Board#board.lastUpdateId,
            activeUsers => [#{
                id => Session#session.userId,
                name => Session#session.userName,
                color => Session#session.color
            } || {_Ref, Session} <- Board#board.sessions],
            canvasObjects => [
                case ObjectType of
                    image -> #{
                        canvasObjectType => <<"Image">>,
                        id => ObjectId,
                        zIndex => Object#image.zIndex,
                        position => utility:vector2_to_map(Object#image.position),
                        width => Object#image.width,
                        size => utility:vector2_to_map(Object#image.size),
                        blobId => Object#image.blobId};
                    curve -> #{
                        canvasObjectType => <<"Curve">>,
                        id => ObjectId,
                        zIndex => Object#drawing_curve.zIndex,
                        points => [
                            utility:vector2_to_map(Point)
                        || Point <- Object#drawing_curve.points],
                        color => Object#drawing_curve.color};
                    stickyNote -> #{
                        canvasObjectType => <<"StickyNote">>,
                        id => ObjectId,
                        zIndex => Object#stickyNote.zIndex,
                        position => utility:vector2_to_map(Object#stickyNote.position),
                        color => Object#stickyNote.color,
                        text => Object#stickyNote.text};
                    comment -> #{
                        canvasObjectType => <<"Comment">>,
                        id => ObjectId,
                        text => Object#comment.text,
                        timestamp => Object#comment.timestamp,
                        imageId => Object#comment.imageId}
                end
            || {ObjectId, ObjectType, Object} <- Board#board.objects]
        }
    ).

% session requests

-spec new_session(Pid :: pid()) ->
    {ok, SessionToken :: binary(), UserId :: binary(),
        UserName :: binary(), Color :: binary(), SessionRef :: reference()}.
new_session(Pid) ->
    gen_server:call(Pid, new_session).

-spec continue_session(Pid :: pid(), SessionToken :: binary()) ->
    {ok, SessionToken :: binary(), UserId :: binary(),
        UserName :: binary(), Color :: binary(), SessionRef :: reference()}.
continue_session(Pid, SessionToken) ->
    gen_server:call(Pid, {continue_session, SessionToken}).

-spec end_session(
    Pid :: pid(),
    SessionToken :: binary(),
    Reason :: {userLeftPermanently | userLeftTemporarily, any()}) -> ok.
end_session(Pid, SessionRef, Reason) ->
    gen_server:call(Pid, {end_session, SessionRef, Reason}).

% reservation and update requests

-spec reserve_canvas_object(Pid :: pid(), CanvasObjectId :: binary(), SessionRef :: binary()) ->
    {ok, ReservationId :: binary(), ExpirationTimeStamp :: integer()} | {error, binary()}.
reserve_canvas_object(Pid, CanvasObjectId, SessionRef) ->
    gen_server:call(Pid, {reserve_canvas_object, CanvasObjectId, SessionRef}).

% already_reserved -> <<"This object is already reserved.">>;
% object_not_found -> <<"This object is not found.">>;
% session_not_found -> <<"This session does not exist.">>;
% board_not_found -> <<"The board does not exist.">>;
% _ -> <<"Unexpected behaviour.">>

-spec update_board(Pid :: pid(), Update :: binary(), SessionRef :: binary()) -> ok | {error, binary()}.
update_board(Pid, Update, SessionRef) ->
    gen_server:call(Pid, {update_board, Update, SessionRef}).

cancel_reservation(Pid, ReservationId, SessionRef) ->
    gen_server:cast(Pid, {cancel_reservation, ReservationId, SessionRef}).

% undo and redo requests

undo(Pid, SessionRef) ->
    gen_server:cast(Pid, {undo, SessionRef}).

redo(Pid, SessionRef) ->
    gen_server:cast(Pid, {redo, SessionRef}).

start_link(BoardId, SupervisorPid) ->
    gen_server:start_link(?MODULE, {BoardId, SupervisorPid}, []).

%% Callback functions
init({BoardId, SupervisorPid}) ->
    %process_flag(trap_exit, true),
    boards_manager_service:request_to_be_registered_and_monitored(BoardId, ?MODULE, self()),
    ObjectsTable = ets:new(board_objects_table, [set]),
    SessionsTable = ets:new(board_sessions_table, [set]),
    TokensTable = ets:new(board_session_tokens_table, [set]),
    Name = BoardId,
    LastUpdateId = 0,
    % We should schedule load_from_database to be executed as the first message
    % We can not call directly because it will cause a deadlock as load_from_database will call
    % get_board_service_pid of the supervisor which is waiting for init to return
    spawn(fun() -> gen_server:cast(self(), load_from_database) end),
    lager:info("Started board controller service for board ~p at ~p", [BoardId, self()]),
    {ok, #state{
        board_id = BoardId,
        supervisor_pid = SupervisorPid,
        name = Name,
        last_update_id = LastUpdateId,
        board_sessions_table = SessionsTable,
        board_session_tokens_table = TokensTable,
        board_objects_table = ObjectsTable
    }}.

handle_call(get_board_state, _From, State) ->
    Sessions = ets:tab2list(State#state.board_sessions_table),
    Objects = ets:tab2list(State#state.board_objects_table),
    Board = #board{
        id = State#state.board_id,
        name = State#state.name,
        lastUpdateId = State#state.last_update_id,
        sessions = [Session || Session <- Sessions],
        objects = [Object || Object <- Objects]
    },
    {reply, Board, State};
handle_call(new_session, _From = {WsPid, _}, State) ->
    Reply = {ok, Session, SessionRef} = create_new_session(State#state.board_sessions_table,
        State#state.board_session_tokens_table, WsPid),
    broadcast_user_joined_to_all_except_ref(Session, SessionRef, State#state.board_sessions_table),
    {reply, Reply, State};
handle_call({continue_session, SessionToken}, _From = {WsPid, _}, State) ->
    case ets:lookup(State#state.board_session_tokens_table, SessionToken) of
        [] ->
            Reply = {ok, Session, SessionRef} = create_new_session(State#state.board_sessions_table,
                State#state.board_session_tokens_table, WsPid),
            broadcast_user_joined_to_all_except_ref(Session, SessionRef, State#state.board_sessions_table),
            {reply, Reply, State};
        [{SessionToken, SessionRef}] ->
            case ets:lookup(State#state.board_sessions_table, SessionRef) of
                [{SessionRef, OldSession}] ->
                    NewSession = OldSession#session{wsPid = WsPid},
                    true = ets:insert(State#state.board_sessions_table, {SessionRef, NewSession}),
                    broadcast_user_joined_to_all_except_ref(NewSession, SessionRef, State#state.board_sessions_table),
                    {reply, {ok, NewSession, SessionRef}, State}
            end
    end;
handle_call({end_session, SessionRef, Reason}, _From, State) ->
    case ets:lookup(State#state.board_sessions_table, SessionRef) of
        [] ->
            {reply, {error, <<"session not found">>}, State};
        [{SessionRef, OldSession}] ->
            case Reason of
                {userLeftPermanently, _} ->
                    true = ets:delete(State#state.board_session_tokens_table, OldSession#session.sessionToken),
                    true = ets:delete(State#state.board_sessions_table, SessionRef),
                    broadcast_user_left_to_all_except_ref(OldSession, SessionRef, State#state.board_sessions_table),
                    {reply, ok, State};
                {userLeftTemporarily, _} ->
                    NewSession = OldSession#session{status = offline, wsPid = undefined},
                    true = ets:insert(State#state.board_sessions_table, {SessionRef, NewSession}),
                    broadcast_user_left_to_all_except_ref(NewSession, SessionRef, State#state.board_sessions_table),
                    {reply, ok, State}
            end
    end;
handle_call({reserve_canvas_object, CanvasObjectId, SessionRef}, _From, State) ->
    {reply, ok, State};
handle_call({update_board, Update, SessionRef}, _From, State) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(load_from_database, State) ->
    load_from_database(State#state.supervisor_pid, State#state.board_objects_table),
    {noreply, State};
handle_cast({cancel_reservation, ReservationId, SessionRef}, State) ->
    {noreply, State};
handle_cast({undo, SessionRef}, State) ->
    {noreply, State};
handle_cast({redo, SessionRef}, State) ->
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

broadcast_user_joined_to_all_except_ref(Session, ExceptSessionRef, SessionsTable) ->
    broadcast_event_to_all_sessions_except_ref(#event{
        eventType = <<"userJoined">>,
        eventPayload = create_user_payload(Session)
    }, ExceptSessionRef, SessionsTable).

broadcast_user_left_to_all_except_ref(Session, ExceptSessionRef, SessionsTable) ->
    broadcast_event_to_all_sessions_except_ref(#event{
        eventType = <<"userLeft">>,
        eventPayload = create_user_payload(Session)
    }, ExceptSessionRef, SessionsTable).

create_user_payload(Session) ->
    #user_payload{
        userId = Session#session.userId,
        userName = Session#session.userName,
        color = Session#session.color
    }.

broadcast_event_to_all_sessions_except_ref(Event, ExceptSessionRef, SessionsTable) ->
    ets:foldl(
        fun({Ref, Session}, _Acc) -> send_event_to_session_except_ref(Event, Ref, Session, ExceptSessionRef) end,
        ok, SessionsTable).

send_event_to_session_except_ref(_, X, _, X) -> skip;
send_event_to_session_except_ref(Event, _Ref, Session, _ExceptSessionRef) ->
    Session#session.wsPid ! {broadcast, Event}, ok.

create_new_session(BoardSessionsTable, BoardSessionTokensTable, WsPid) ->
    SessionToken = utility:new_uuid(),
    UserId = utility:new_uuid(),
    UserName = utility:new_anonymous_animal_name(),
    Color = utility:new_color(),
    SessionRef = make_ref(),
    Session = #session{
        userId = UserId,
        userName = UserName,
        color = Color,
        sessionToken = SessionToken,
        wsPid = WsPid,
        status = online,
        undoStack = [],
        redoStack = []
    },
    true = ets:insert(BoardSessionsTable, {SessionRef, Session}),
    true = ets:insert(BoardSessionTokensTable, {SessionToken, SessionRef}),
    {ok, Session, SessionRef}.

load_from_database(SupervisorPid, ObjectsTable) ->
    DatabaseServicePid = board_sup:get_board_service_pid(SupervisorPid, board_database_service),
    Objects = board_database_service:get_all_tables(DatabaseServicePid), % {Objects, Sessions, Tokens}
    true = ets:insert(ObjectsTable, Objects).
    %ets:insert(SessionsTable, Sessions),
    %ets:insert(TokensTable, Tokens).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-endif.