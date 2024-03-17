-module(websocket_event_handlers).

-include("records.hrl").

-export([
    handle_event/2
]).


handle_event(
        #event{eventType = 'begin', eventId = _EventId, eventContent = Event}, 
        #websocket_handler_state{boardManagerPid = Pid} = State) ->
    case {Event#begin_event.sessionType, Event#begin_event.sessionToken, Event#begin_event.lastEventId} of
        {new, _, _} -> 
            {ok, SessionToken} = board_controller_service:new_session(Pid),
            {[{active, true}], State#websocket_handler_state{sessionToken = SessionToken}};
        {continue, undefined, _} -> 
            {[{close, <<"session token is not provided">>}], State};
        {continue, undefined, undefined} -> 
            {[{close, <<"last event id is not provided">>}], State};
        {continue, SessionToken0, LastEventId} when is_integer(LastEventId), is_binary(SessionToken0) -> 
            {ok, SessionToken1} = board_controller_service:continue_session(Pid, SessionToken0, LastEventId),
            {[{active, true}], State#websocket_handler_state{sessionToken = SessionToken1}};
        {continue, _, _} ->
            {[{close, <<"malformed begin event">>}], State};
        {undefined, _, _} -> 
            {[{close, <<"invalid session type">>}], State}
    end.