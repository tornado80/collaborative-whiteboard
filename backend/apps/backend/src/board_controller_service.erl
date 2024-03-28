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
    end_session/3,
    get_state/1,
    apply_erasing_curves_to_canvas/2
]).

%% State record
-record(state, {
    board_id :: binary(),
    name :: binary(),
    user_count :: integer(),
    board_sessions_table :: ets:tid(),
    board_session_tokens_table :: ets:tid(),
    board_objects_table :: ets:tid(),
    last_update_id :: integer(),
    supervisor_pid :: pid(),
    reservations_table :: ets:tid(),
    curves_updater_timer_ref :: reference(),
    inactivity_timer_ref :: reference(),
    shutdown_scheduled :: boolean()
}).

%% API functions

-spec get_board_state(Pid :: pid()) -> Json :: binary().
get_board_state(Pid) ->
    Board = gen_server:call(Pid, get_board_state),
    jsone:encode(
        #{
            id => Board#board.id,
            lastUpdateId => Board#board.lastUpdateId,
            onlineUsers => [#{
                id => Session#session.userId,
                name => Session#session.userName,
                color => Session#session.color
            } || {_Ref, Session} <- Board#board.sessions, Session#session.status =:= online],
            canvasObjects => [
                case ObjectType of
                    image -> #{
                        canvasObjectType => <<"Image">>,
                        id => ObjectId,
                        zIndex => Object#image.zIndex,
                        position => Object#image.position,
                        width => Object#image.width,
                        size => Object#image.size,
                        blobId => Object#image.blobId};
                    drawingCurve -> #{
                        canvasObjectType => <<"DrawCurve">>,
                        id => ObjectId,
                        points => Object#drawing_curve.points,
                        color => Object#drawing_curve.color};
                    erasingCurve -> #{
                        canvasObjectType => <<"EraseCurve">>,
                        id => ObjectId,
                        centers => Object#erasing_curve.centers,
                        radius => Object#erasing_curve.radius};
                    stickyNote -> #{
                        canvasObjectType => <<"StickyNote">>,
                        id => ObjectId,
                        zIndex => Object#stickyNote.zIndex,
                        position => Object#stickyNote.position,
                        color => Object#stickyNote.color,
                        text => Object#stickyNote.text};
                    comment -> #{
                        canvasObjectType => <<"Comment">>,
                        id => ObjectId,
                        text => Object#comment.text,
                        timestamp => Object#comment.timestamp,
                        imageId => Object#comment.imageId}
                end
            || {ObjectId, ObjectType, _ReservationStatus, Object} <- Board#board.objects]
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

-spec update_board(Pid :: pid(), Update0 :: #update_payload{}, SessionRef :: binary()) ->
    {ok, UpdateId :: integer(), Update1 :: #update_payload{}} | {error, binary()}.
update_board(Pid, Update, SessionRef) ->
    gen_server:call(Pid, {update_board, Update, SessionRef}).

cancel_reservation(Pid, ReservationId, SessionRef) ->
    gen_server:cast(Pid, {cancel_reservation, ReservationId, SessionRef}).

% undo and redo requests

undo(Pid, SessionRef) ->
    gen_server:cast(Pid, {undo, SessionRef}).

redo(Pid, SessionRef) ->
    gen_server:cast(Pid, {redo, SessionRef}).

% Practicality

start_link(BoardId, SupervisorPid) ->
    gen_server:start_link(?MODULE, {BoardId, SupervisorPid}, []).

get_state(Pid) ->
    gen_server:call(Pid, get_state).

%% Callback functions
init({BoardId, SupervisorPid}) ->
    process_flag(trap_exit, true),
    boards_manager_service:request_to_be_registered_and_monitored(BoardId, ?MODULE, self()),
    ObjectsTable = ets:new(board_objects_table, [set]),
    % TODO: We have to keep sessions and reservations table in memory between crashes
    SessionsTable = ets:new(board_sessions_table, [set]),
    TokensTable = ets:new(board_session_tokens_table, [set]),
    ReservationsTable = ets:new(board_reservations_table, [set]),
    Name = BoardId,
    % TODO: We have to keep this counter monotonically increasing between crashes
    LastUpdateId = 0,
    % We should schedule load_from_database to be executed as the first message
    % We can not call directly because it will cause a deadlock as load_from_database will call
    % get_board_service_pid of the supervisor which is waiting for init to return
    gen_server:cast(self(), load_objects_from_database),
    % this will remove erasing curves and split drawing curves into smaller curves according to erasing paths
    % then it will push the result to the controller (to update objects_table) and database
    % update_curves will be called every 10 seconds in another process and only has read-access to objects_table
    {ok, CurvesUpdaterTimerRef} = timer:apply_interval(10000, ?MODULE, apply_erasing_curves_to_canvas, [self(), ObjectsTable]),
    lager:info("Started board controller service for board ~p at ~p", [BoardId, self()]),
    {ok, #state{
        board_id = BoardId,
        supervisor_pid = SupervisorPid,
        name = Name,
        last_update_id = LastUpdateId,
        user_count = 0,
        board_sessions_table = SessionsTable,
        board_session_tokens_table = TokensTable,
        board_objects_table = ObjectsTable,
        reservations_table = ReservationsTable,
        curves_updater_timer_ref = CurvesUpdaterTimerRef,
        inactivity_timer_ref = do_schedule_board_inactivity_timer(),
        shutdown_scheduled = true
    }}.

handle_call(get_state, _From, State) ->
    {reply, State, State};
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
    NewState = cancel_board_inactivity_timer(State),
    Reply = {ok, Session, SessionRef} = create_new_session(State#state.board_sessions_table,
        State#state.board_session_tokens_table, WsPid),
    broadcast_user_joined_to_all_except_ref(Session, SessionRef, State#state.board_sessions_table),
    {reply, Reply, NewState#state{user_count = NewState#state.user_count + 1}};
handle_call({continue_session, SessionToken}, _From = {WsPid, _}, State) ->
    NewState = cancel_board_inactivity_timer(State),
    case ets:lookup(State#state.board_session_tokens_table, SessionToken) of
        [] ->
            Reply = {ok, Session, SessionRef} = create_new_session(State#state.board_sessions_table,
                State#state.board_session_tokens_table, WsPid),
            broadcast_user_joined_to_all_except_ref(Session, SessionRef, State#state.board_sessions_table),
            {reply, Reply, NewState#state{user_count = State#state.user_count + 1}};
        [{SessionToken, SessionRef}] ->
            case ets:lookup(State#state.board_sessions_table, SessionRef) of
                [{SessionRef, OldSession0}] ->
                    OldSession1 = cancel_session_inactivity_timer(OldSession0),
                    NewSession = OldSession1#session{wsPid = WsPid, status = online},
                    true = ets:insert(State#state.board_sessions_table, {SessionRef, NewSession}),
                    broadcast_user_joined_to_all_except_ref(NewSession, SessionRef, State#state.board_sessions_table),
                    {reply, {ok, NewSession, SessionRef}, NewState}
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
                    NewState = schedule_board_inactivity_timer_if_needed(State),
                    {reply, ok, NewState#state{user_count = NewState#state.user_count - 1}};
                {userLeftTemporarily, _} ->
                    OldSession1 = schedule_session_inactivity_timer(OldSession, SessionRef),
                    NewSession = OldSession1#session{status = offline, wsPid = undefined},
                    true = ets:insert(State#state.board_sessions_table, {SessionRef, NewSession}),
                    broadcast_user_left_to_all_except_ref(NewSession, SessionRef, State#state.board_sessions_table),
                    {reply, ok, State}
            end
    end;
handle_call({reserve_canvas_object, CanvasObjectId, SessionRef}, _From, State) ->
    case ets:lookup(State#state.board_objects_table, CanvasObjectId) of
        [] ->
            {reply, {error, <<"object not found">>}, State};
        [{CanvasObjectId, ObjectType, ReservationStatus, Object}] ->
            case ReservationStatus of
                {reserved, _Ref} ->
                    {reply, {error, <<"already reserved">>}, State};
                not_reserved ->
                    case ObjectType of
                        curve -> {reply, {error, <<"curves do not need reservation">>}, State};
                        _ ->
                            case ets:lookup(State#state.board_sessions_table, SessionRef) of
                                [] ->
                                    {reply, {error, <<"session not found">>}, State};
                                [{SessionRef, _Session}] ->
                                    NewReservationStatus = {reserved, SessionRef},
                                    NewEntry = {CanvasObjectId, ObjectType, NewReservationStatus, Object},
                                    true = ets:insert(State#state.board_objects_table, NewEntry),
                                    ReservationId = utility:new_uuid(),
                                    Period = 10000,
                                    {ok, TimerRef} = timer:send_after(Period,
                                        {auto_expire_reservation, ReservationId, CanvasObjectId, SessionRef}),
                                    true = ets:insert(State#state.reservations_table,
                                        {ReservationId, CanvasObjectId, TimerRef}),
                                    broadcast_object_reserved_to_all_sessions_except_ref(ReservationId, SessionRef,
                                        State#state.board_sessions_table),
                                    {reply, {ok, ReservationId, erlang:system_time(millisecond) + Period}}
                            end
                    end
            end
    end,
    {reply, ok, State};
handle_call({update_board, UpdatePayload, SessionRef}, _From, State) ->
    lager:info("Update board: ~p", [UpdatePayload]),
    case ets:lookup(State#state.board_sessions_table, SessionRef) of
        [] ->
            {reply, {error, <<"session not found">>}, State};
        [{SessionRef, Session}] ->
            NewUpdateId = State#state.last_update_id + 1,
            NewState = State#state{last_update_id = NewUpdateId},
            PropList = (UpdatePayload#update_payload.operation)#canvas_object_operation.canvasObjectOperationPayload,
            case UpdatePayload#update_payload.canvasObjectType of
                canvas ->
                    case UpdatePayload#update_payload.operationType of
                        draw ->
                            ObjectId = utility:new_uuid(),
                            Object = #drawing_curve{
                                points = proplists:get_value(<<"points">>, PropList),
                                color = proplists:get_value(<<"color">>, PropList)
                            },
                            insert_object_into_ets_and_database(State#state.supervisor_pid,
                                State#state.board_objects_table, ObjectId, drawingCurve, Object),
                            NewUpdatePayload = UpdatePayload#update_payload{
                                canvasObjectId = ObjectId
                            },
                            broadcast_board_update_to_all_sessions_except_ref(NewUpdatePayload, NewUpdateId,
                                Session, SessionRef, State#state.board_sessions_table),
                            {reply, {ok, NewUpdateId, NewUpdatePayload}, NewState};
                        erase ->
                            ObjectId = utility:new_uuid(),
                            Object = #erasing_curve{
                                centers = proplists:get_value(<<"centers">>, PropList),
                                radius = proplists:get_value(<<"radius">>, PropList)
                            },
                            insert_object_into_ets_and_database(State#state.supervisor_pid,
                                State#state.board_objects_table, ObjectId, erasingCurve, Object),
                            NewUpdatePayload = UpdatePayload#update_payload{
                                canvasObjectId = ObjectId
                            },
                            broadcast_board_update_to_all_sessions_except_ref(NewUpdatePayload, NewUpdateId,
                                Session, SessionRef, State#state.board_sessions_table),
                            {reply, {ok, NewUpdateId, NewUpdatePayload}, NewState};
                        _ ->
                            {reply, {error, <<"invalid operation type">>}, State}
                    end;
                _ ->
                    case UpdatePayload#update_payload.operationType of
                        create ->
                            ObjectId = utility:new_uuid(),
                            ObjectType = UpdatePayload#update_payload.canvasObjectType,
                            Object = case ObjectType of
                                stickyNote ->
                                    #stickyNote{
                                        zIndex = proplists:get_value(<<"zIndex">>, PropList),
                                        position = proplists:get_value(<<"position">>, PropList),
                                        color = proplists:get_value(<<"color">>, PropList),
                                        text = proplists:get_value(<<"text">>, PropList)
                                    };
                                comment ->
                                    #comment{
                                        text = proplists:get_value(<<"text">>, PropList),
                                        timestamp = proplists:get_value(<<"timestamp">>, PropList),
                                        imageId = proplists:get_value(<<"imageId">>, PropList)
                                    };
                                image ->
                                    #image{
                                        zIndex = proplists:get_value(<<"zIndex">>, PropList),
                                        position = proplists:get_value(<<"position">>, PropList),
                                        width = proplists:get_value(<<"width">>, PropList),
                                        size = proplists:get_value(<<"size">>, PropList),
                                        blobId = proplists:get_value(<<"blobId">>, PropList)
                                    }
                            end,
                            insert_object_into_ets_and_database(State#state.supervisor_pid,
                                State#state.board_objects_table, ObjectId, ObjectType, Object),
                            NewUpdatePayload = UpdatePayload#update_payload{
                                canvasObjectId = ObjectId
                            },
                            broadcast_board_update_to_all_sessions_except_ref(NewUpdatePayload, NewUpdateId,
                                Session, SessionRef, State#state.board_sessions_table),
                            Update = #update{
                                id = NewUpdateId,
                                objectId = ObjectId,
                                objectType = UpdatePayload#update_payload.canvasObjectType,
                                operationType = create,
                                oldValue = undefined,
                                newValue = Object
                            },
                            NewSession = Session#session{
                                undoStack = [Update | Session#session.undoStack], % push to undo stack
                                redoStack = [] % clear redo stack
                            },
                            ets:insert(State#state.board_sessions_table, {SessionRef, NewSession}),
                            {reply, {ok, NewUpdateId, NewUpdatePayload}, NewState};
                        X when X =:= update; X =:= delete ->
                            ObjectId = UpdatePayload#update_payload.canvasObjectId,
                            case ets:lookup(State#state.board_objects_table, ObjectId) of
                                [] ->
                                    {reply, {error, <<"object id not found">>}, State};
                                [{ObjectId, ObjectType, {reserved, SessionRef}, Object}] ->
                                    case X of
                                        delete ->
                                            delete_object_from_ets_and_database(State#state.supervisor_pid,
                                                State#state.board_objects_table, ObjectId),
                                            NewUpdatePayload = UpdatePayload#update_payload{
                                                canvasObjectId = ObjectId
                                            },
                                            broadcast_board_update_to_all_sessions_except_ref(NewUpdatePayload, NewUpdateId,
                                                Session, SessionRef, State#state.board_sessions_table),
                                            Update = #update{
                                                id = NewUpdateId,
                                                objectId = ObjectId,
                                                objectType = ObjectType,
                                                operationType = delete,
                                                oldValue = Object,
                                                newValue = undefined
                                            },
                                            NewSession = Session#session{
                                                undoStack = [Update | Session#session.undoStack], % push to undo stack
                                                redoStack = [] % clear redo stack
                                            },
                                            ets:insert(State#state.board_sessions_table, {SessionRef, NewSession}),
                                            {reply, {ok, NewUpdateId, NewUpdatePayload}, NewState};
                                        update ->
                                            NewObject = case ObjectType of
                                                stickyNote ->
                                                    Object#stickyNote{
                                                        zIndex = proplists:get_value(<<"zIndex">>, PropList, Object#stickyNote.zIndex),
                                                        position = proplists:get_value(<<"position">>, PropList, Object#stickyNote.position),
                                                        color = proplists:get_value(<<"color">>, PropList, Object#stickyNote.color),
                                                        text = proplists:get_value(<<"text">>, PropList, Object#stickyNote.text)
                                                    };
                                                comment ->
                                                    Object#comment{
                                                        text = proplists:get_value(<<"text">>, PropList, Object#comment.text),
                                                        timestamp = proplists:get_value(<<"timestamp">>, PropList, Object#comment.timestamp),
                                                        imageId = proplists:get_value(<<"imageId">>, PropList, Object#comment.imageId)
                                                    };
                                                image ->
                                                    Object#image{
                                                        zIndex = proplists:get_value(<<"zIndex">>, PropList, Object#image.zIndex),
                                                        position = proplists:get_value(<<"position">>, PropList, Object#image.position),
                                                        width = proplists:get_value(<<"width">>, PropList, Object#image.width),
                                                        size = proplists:get_value(<<"size">>, PropList, Object#image.size),
                                                        blobId = proplists:get_value(<<"blobId">>, PropList, Object#image.blobId)
                                                    }
                                            end,
                                            insert_object_into_ets_and_database(State#state.supervisor_pid,
                                                State#state.board_objects_table, ObjectId, ObjectType, NewObject),
                                            NewUpdatePayload = UpdatePayload#update_payload{
                                                canvasObjectId = ObjectId
                                            },
                                            broadcast_board_update_to_all_sessions_except_ref(NewUpdatePayload, NewUpdateId,
                                                Session, SessionRef, State#state.board_sessions_table),
                                            Update = #update{
                                                id = NewUpdateId,
                                                objectId = ObjectId,
                                                objectType = ObjectType,
                                                operationType = update,
                                                oldValue = Object,
                                                newValue = NewObject
                                            },
                                            NewSession = Session#session{
                                                undoStack = [Update | Session#session.undoStack], % push to undo stack
                                                redoStack = [] % clear redo stack
                                            },
                                            ets:insert(State#state.board_sessions_table, {SessionRef, NewSession}),
                                            {reply, {ok, NewUpdateId, NewUpdatePayload}, NewState}
                                    end;
                                _ ->
                                    {reply, {error, <<"object is not reserved by the session">>}, State}
                            end;
                        _ ->
                            {reply, {error, <<"invalid operation type">>}, State}
                    end
            end
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(load_objects_from_database, State) ->
    lager:info("Board ~p controller service is loading objects from database", [State#state.board_id]),
    load_objects_from_database(State#state.supervisor_pid, State#state.board_objects_table),
    lager:info("Board ~p controller service loaded objects from database", [State#state.board_id]),
    {noreply, State};
handle_cast({cancel_reservation, ReservationId, SessionRef}, State) ->
    case ets:lookup(State#state.reservations_table, ReservationId) of
        [] -> ok;
        [{ReservationId, CanvasObjectId, TimerRef}] ->
            {ok, cancel} = timer:cancel(TimerRef),
            true = ets:delete(State#state.reservations_table, ReservationId),
            case ets:lookup(State#state.board_objects_table, CanvasObjectId) of
                [] -> ok;
                [{CanvasObjectId, ObjectType, {reserved, SessionRef}, Object}] ->
                    NewReservationStatus = not_reserved,
                    NewEntry = {CanvasObjectId, ObjectType, NewReservationStatus, Object},
                    true = ets:insert(State#state.board_objects_table, NewEntry),
                    broadcast_reservation_cancelled_to_all_sessions(ReservationId, State#state.board_sessions_table);
                _ -> ok
            end
    end,
    {noreply, State};
handle_cast({undo, SessionRef}, State) ->
    case ets:lookup(State#state.board_sessions_table, SessionRef) of
        [] ->
            {noreply, State};
        [{SessionRef, Session}] ->
            case Session#session.undoStack of
                [] ->
                    {noreply, State};
                [Top | Rest] ->
                    NewUpdateId = State#state.last_update_id + 1,
                    NewState = State#state{last_update_id = NewUpdateId},
                    NewValue = Top#update.newValue,
                    case ets:lookup(State#state.board_objects_table, Top#update.objectId) of
                       [] ->
                           case Top#update.operationType of
                               delete ->
                                   UpdatePayload = to_payload(Top#update{operationType = create}, Top#update.oldValue), % we are undoing :)
                                   broadcast_board_update_to_all_sessions_except_ref(UpdatePayload, NewUpdateId, Session, SessionRef,
                                       State#state.board_sessions_table),
                                   ets:insert(State#state.board_sessions_table, {SessionRef, Session#session{
                                       undoStack = Rest,
                                       redoStack = [Top | Session#session.redoStack]}}),
                                   insert_object_into_ets_and_database(State#state.supervisor_pid, State#state.board_objects_table,
                                       Top#update.objectId, Top#update.objectType, Top#update.oldValue),
                                   {noreply, NewState};
                               _ ->
                                   % undo not allowed and the last update is ignored (current state does not match with
                                   % the new value of top update)
                                   ets:insert(State#state.board_sessions_table, {SessionRef, Session#session{undoStack = Rest}}),
                                   {noreply, State}
                           end;
                       [{ObjectId, ObjectType, {reserved, SessionRef}, NewValue}] ->
                           case Top#update.operationType of
                               create ->
                                    UpdatePayload = to_payload(Top#update{operationType = delete}, Top#update.newValue), % we are undoing :)
                                    broadcast_board_update_to_all_sessions_except_ref(UpdatePayload,
                                        NewUpdateId, Session, SessionRef, State#state.board_sessions_table),
                                    delete_object_from_ets_and_database(State#state.supervisor_pid, State#state.board_objects_table,
                                        Top#update.objectId),
                                    ets:insert(State#state.board_sessions_table, {SessionRef, Session#session{
                                        undoStack = Rest,
                                        redoStack = [Top | Session#session.redoStack]}}),
                                    {noreply, NewState};
                               update ->
                                   UpdatePayload = to_payload(Top, Top#update.oldValue),
                                   broadcast_board_update_to_all_sessions_except_ref(UpdatePayload,
                                       NewUpdateId, Session, SessionRef, State#state.board_sessions_table),
                                   ets:insert(State#state.board_sessions_table, {SessionRef, Session#session{
                                       undoStack = Rest,
                                       redoStack = [Top | Session#session.redoStack]}}),
                                   insert_object_into_ets_and_database(State#state.supervisor_pid, State#state.board_objects_table,
                                       ObjectId, ObjectType, Top#update.oldValue),
                                   {noreply, NewState};
                               _ ->
                                   % undo not allowed and the last update is ignored (current state does not match with
                                   % the new value of top update)
                                   ets:insert(State#state.board_sessions_table, {SessionRef, Session#session{undoStack = Rest}}),
                                   {noreply, State}
                           end;
                       [{_ObjectId, _ObjectType, {reserved, SessionRef}, _}] ->
                           % undo not allowed and the last update is ignored (current state does not match with
                           % the new value of top update)
                           ets:insert(State#state.board_sessions_table, {SessionRef, Session#session{undoStack = Rest}}),
                           {noreply, State};
                       _ ->
                           % object is currently reserved for some one else
                           {noreply, State}
                    end
            end
    end;
handle_cast({redo, SessionRef}, State) ->
    case ets:lookup(State#state.board_sessions_table, SessionRef) of
        [] ->
            {noreply, State};
        [{SessionRef, Session}] ->
            case Session#session.redoStack of
                [] ->
                    {noreply, State};
                [Top | Rest] ->
                    NewUpdateId = State#state.last_update_id + 1,
                    NewState = State#state{last_update_id = NewUpdateId},
                    OldValue = Top#update.oldValue,
                    case ets:lookup(State#state.board_objects_table, Top#update.objectId) of
                        [{ObjectId, ObjectType, {reserved, SessionRef}, OldValue}] ->
                            case Top#update.operationType of
                                delete ->
                                    UpdatePayload = to_payload(Top#update{operationType = delete}, Top#update.oldValue), % we are redoing :)
                                    broadcast_board_update_to_all_sessions_except_ref(UpdatePayload, NewUpdateId, Session, SessionRef,
                                        State#state.board_sessions_table),
                                    delete_object_from_ets_and_database(State#state.supervisor_pid, State#state.board_objects_table,
                                        Top#update.objectId),
                                    ets:insert(State#state.board_sessions_table, {SessionRef, Session#session{
                                        undoStack = [Top | Session#session.undoStack],
                                        redoStack = Rest}}),
                                    {noreply, NewState};
                                update ->
                                    UpdatePayload = to_payload(Top, Top#update.newValue),
                                    broadcast_board_update_to_all_sessions_except_ref(UpdatePayload,
                                        NewUpdateId, Session, SessionRef, State#state.board_sessions_table),
                                    ets:insert(State#state.board_sessions_table, {SessionRef, Session#session{
                                        undoStack = [Top | Session#session.undoStack],
                                        redoStack = Rest}}),
                                    insert_object_into_ets_and_database(State#state.supervisor_pid, State#state.board_objects_table,
                                        ObjectId, ObjectType, Top#update.oldValue),
                                    {noreply, NewState};
                                _ ->
                                    % undo not allowed and the last update is ignored (current state does not match with
                                    % the new value of top update)
                                    ets:insert(State#state.board_sessions_table, {SessionRef, Session#session{redoStack = Rest}}),
                                    {noreply, State}
                            end;
                        [] ->
                            case Top#update.operationType of
                                create ->
                                    UpdatePayload = to_payload(Top#update{operationType = create}, Top#update.newValue), % we are redoing :)
                                    broadcast_board_update_to_all_sessions_except_ref(UpdatePayload,
                                        NewUpdateId, Session, SessionRef, State#state.board_sessions_table),
                                    insert_object_into_ets_and_database(State#state.supervisor_pid, State#state.board_objects_table,
                                        Top#update.objectId, Top#update.objectType, Top#update.newValue),
                                    ets:insert(State#state.board_sessions_table, {SessionRef, Session#session{
                                        undoStack = [Top | Session#session.undoStack],
                                        redoStack = Rest}}),
                                    {noreply, NewState};
                                _ ->
                                    % undo not allowed and the last update is ignored (current state does not match with
                                    % the new value of top update)
                                    ets:insert(State#state.board_sessions_table, {SessionRef, Session#session{redoStack = Rest}}),
                                    {noreply, State}
                            end;
                        [{_ObjectId, _ObjectType, {reserved, SessionRef}, _}] ->
                            % undo not allowed and the last update is ignored (current state does not match with
                            % the new value of top update)
                            ets:insert(State#state.board_sessions_table, {SessionRef, Session#session{redoStack = Rest}}),
                            {noreply, State};
                        _ ->
                            % object is currently reserved for some one else
                            {noreply, State}
                    end
            end
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({auto_expire_reservation, ReservationId, CanvasObjectId, SessionRef}, State) ->
    case ets:lookup(State#state.reservations_table, ReservationId) of
        [] -> ok; % reservation was cancelled before
        [{ReservationId, CanvasObjectId, _TimerRef}] ->
            true = ets:delete(State#state.reservations_table, ReservationId);
        _ -> ok
    end,
    case ets:lookup(State#state.board_objects_table, CanvasObjectId) of
        [] -> ok; % strange case
        [{CanvasObjectId, ObjectType, {reserved, SessionRef}, Object}] ->
            NewReservationStatus = not_reserved,
            NewEntry = {CanvasObjectId, ObjectType, NewReservationStatus, Object},
            true = ets:insert(State#state.board_objects_table, NewEntry),
            broadcast_reservation_expired_to_all_sessions(ReservationId, State#state.board_sessions_table);
        _ -> ok
    end,
    {noreply, State};
handle_info(shutdown, State = #state{shutdown_scheduled = true}) ->
    lager:info("Shutting down board controller service for board ~p at ~p", [State#state.board_id, self()]),
    {ok, cancel} = timer:cancel(State#state.curves_updater_timer_ref),
    do_apply_erasing_curves_to_canvas(State#state.board_objects_table),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

apply_erasing_curves_to_canvas(_Pid, ObjectsTable) ->
    do_apply_erasing_curves_to_canvas(ObjectsTable).

do_apply_erasing_curves_to_canvas(_ObjectsTable) ->
    % TODO: implement erasing curves
    ok.

insert_object_into_ets_and_database(SupervisorPid, ObjectsTable, ObjectId, ObjectType, Object) ->
    true = ets:insert(ObjectsTable, {ObjectId, ObjectType, not_reserved, Object}),
    insert_object_into_database(SupervisorPid, ObjectId, ObjectType, Object).

delete_object_from_ets_and_database(SupervisorPid, ObjectsTable, ObjectId) ->
    ets:delete(ObjectsTable, ObjectId),
    delete_object_from_database(SupervisorPid, ObjectId).

broadcast_board_update_to_all_sessions_except_ref(
        UpdatePayload, UpdateId, ExceptSession, ExceptSessionRef, SessionsTable) ->
    broadcast_event_to_all_sessions_except_ref(#event{
        eventType = <<"boardUpdated">>,
        eventPayload = #board_updated_payload{
            updateId = UpdateId,
            userId = ExceptSession#session.userId,
            intermediate = false,
            update = UpdatePayload
        }
    }, ExceptSessionRef, SessionsTable).

broadcast_object_reserved_to_all_sessions_except_ref(ReservationId, ExceptSessionRef, SessionsTable) ->
    broadcast_event_to_all_sessions_except_ref(#event{
        eventType = <<"canvasObjectReserved">>,
        eventPayload = #canvas_object_reserved_payload{
            reservationId = ReservationId
        }
    }, ExceptSessionRef, SessionsTable).

broadcast_reservation_cancelled_to_all_sessions(ReservationId, SessionsTable) ->
    broadcast_event_to_all_sessions(#event{
        eventType = <<"reservationCancelled">>,
        eventPayload = #reservation_cancelled_payload{
            reservationId = ReservationId
        }
    }, SessionsTable).

broadcast_reservation_expired_to_all_sessions(ReservationId, SessionsTable) ->
    broadcast_event_to_all_sessions(#event{
        eventType = <<"reservationExpired">>,
        eventPayload = #reservation_expired_payload{
            reservationId = ReservationId
        }
    }, SessionsTable).

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

broadcast_event_to_all_sessions(Event, SessionsTable) ->
    broadcast_event_to_all_sessions_except_ref(Event, undefined, SessionsTable).

broadcast_event_to_all_sessions_except_ref(Event, ExceptSessionRef, SessionsTable) ->
    ets:foldl(
        fun({Ref, Session}, _Acc) -> send_event_to_session_except_ref(Event, Ref, Session, ExceptSessionRef) end,
        ok, SessionsTable).

send_event_to_session_except_ref(_, X, _, X) -> skip;
send_event_to_session_except_ref(Event, _Ref, Session = #session{status = online}, _ExceptSessionRef) ->
    lager:info("Sending event ~p to session ~p", [Event, Session]),
    Session#session.wsPid ! {broadcast, Event}, ok;
send_event_to_session_except_ref(_, _, _, _) -> skip.

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
        redoStack = [],
        inactivityTimerRef = undefined
    },
    true = ets:insert(BoardSessionsTable, {SessionRef, Session}),
    true = ets:insert(BoardSessionTokensTable, {SessionToken, SessionRef}),
    {ok, Session, SessionRef}.

load_objects_from_database(SupervisorPid, ObjectsTable) ->
    DatabaseServicePid = board_sup:get_board_service_pid(SupervisorPid, board_database_service),
    Objects = board_database_service:get_objects_table(DatabaseServicePid),
    true = ets:insert(ObjectsTable, Objects).

insert_object_into_database(SupervisorPid, ObjectId, ObjectType, Object) ->
    DatabaseServicePid = board_sup:get_board_service_pid(SupervisorPid, board_database_service),
    board_database_service:insert_object(DatabaseServicePid, ObjectId, ObjectType, Object).

delete_object_from_database(SupervisorPid, ObjectId) ->
    DatabaseServicePid = board_sup:get_board_service_pid(SupervisorPid, board_database_service),
    board_database_service:delete_object(DatabaseServicePid, ObjectId).

cancel_board_inactivity_timer(State) ->
    case State#state.inactivity_timer_ref of
        undefined -> State;
        InactivityTimerRef ->
            {ok, cancel} = timer:cancel(InactivityTimerRef),
            State#state{
                inactivity_timer_ref = undefined,
                shutdown_scheduled = false}
    end.

schedule_board_inactivity_timer_if_needed(State) ->
    case State#state.user_count of
        1 ->
            InactivityTimerRef = do_schedule_board_inactivity_timer(),
            State#state{inactivity_timer_ref = InactivityTimerRef, shutdown_scheduled = true};
        _ ->
            State
    end.

do_schedule_board_inactivity_timer() ->
    {ok, Ref} = timer:send_after(60000, shutdown),
    Ref.

cancel_session_inactivity_timer(Session) ->
    case Session#session.inactivityTimerRef of
        undefined -> Session;
        InactivityTimerRef ->
            {ok, cancel} = timer:cancel(InactivityTimerRef), Session#session{inactivityTimerRef = undefined}
    end.

schedule_session_inactivity_timer(Session, SessionRef) ->
    {ok, Ref} = timer:apply_after(60000, ?MODULE, end_session,
        [self(), SessionRef, {userLeftPermanently, inactive}]),
    Session#session{inactivityTimerRef = Ref}.

to_payload(Update, UpdateValue) ->
    #update_payload{
        canvasObjectId = Update#update.objectId,
        canvasObjectType = Update#update.objectType,
        operationType = Update#update.operationType,
        operation = #canvas_object_operation{
            canvasObjectOperationType = case {Update#update.objectType, Update#update.operationType} of
                {image, create} -> createImage;
                {image, update} -> updateImage;
                {image, delete} -> deleteImage;
                {stickyNote, create} -> createStickyNote;
                {stickyNote, update} -> updateStickyNote;
                {stickyNote, delete} -> deleteStickyNote;
                {comment, create} -> createComment;
                {comment, update} -> updateComment;
                {comment, delete} -> deleteComment
            end,
            canvasObjectOperationPayload = case Update#update.objectType of
                image -> [
                    {<<"zIndex">>, UpdateValue#image.zIndex},
                    {<<"position">>, UpdateValue#image.position},
                    {<<"width">>, UpdateValue#image.width},
                    {<<"size">>, UpdateValue#image.size},
                    {<<"blobId">>, UpdateValue#image.blobId}
                ];
                stickyNote -> [
                    {<<"zIndex">>, UpdateValue#stickyNote.zIndex},
                    {<<"position">>, UpdateValue#stickyNote.position},
                    {<<"color">>, UpdateValue#stickyNote.color},
                    {<<"text">>, UpdateValue#stickyNote.text}
                ];
                comment -> [
                    {<<"text">>, UpdateValue#comment.text},
                    {<<"timestamp">>, UpdateValue#comment.timestamp},
                    {<<"imageId">>, UpdateValue#comment.imageId}
                ]
            end
        }
    }.

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-endif.