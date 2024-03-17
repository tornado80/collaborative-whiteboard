-module(websocket_event_handlers).

-include("records.hrl").

-export([
    handle_event/2
]).

handle_event(
        #event{eventType = 'begin', eventId = EventId, eventContent = Event}, 
        #websocket_handler_state{boardManagerPid = Pid} = State) ->
    case {Event#begin_event.sessionType, Event#begin_event.sessionToken, Event#begin_event.lastEventId} of
        {new, _, _} -> 
            {ok, SessionToken, UserId} = board_controller_service:new_session(Pid),

            WelcomeData = jsone:encode({[{<<"eventId">>, EventId+1}, {<<"eventType">>, <<"welcomeUser">>},{<<"sessionToken">>, SessionToken}, {<<"userId">>, UserId}]}),
            
            {[{ text, WelcomeData}], State#websocket_handler_state{sessionToken = SessionToken}};
            
        {continue, undefined, _} -> 
            {[{close, <<"session token is not provided">>}], State};
        {continue, _, undefined} -> 
            {[{close, <<"last event id is not provided">>}], State};
        {continue, SessionToken0, LastEventId} when is_integer(LastEventId), is_binary(SessionToken0) -> 
            {ok, SessionToken1, UserId} = board_controller_service:continue_session(Pid, SessionToken0, LastEventId),
             jsone:encode({[{<<"eventId">>, EventId+1}, {<<"eventType">>, <<"welcomeUser">>},{<<"sessionToken">>, SessionToken1}, {<<"userId">>, UserId}]}),

            {[{text, welcomeUser}], State#websocket_handler_state{sessionToken = SessionToken1}};
        {continue, _, _} ->
            {[{close, <<"malformed begin event">>}], State};
        {undefined, _, _} -> 
            {[{close, <<"invalid session type">>}], State}
    end.