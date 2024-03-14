-module(boards_lookup_service).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions
-export([create_board/0]).

%% State record
-record(state, {}).

-define(SERVER, ?MODULE).

%% API functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec create_board() -> {ok, BoardId :: binary()} | {error, Reason :: string()}.
create_board() ->
    % TODO: perhaps gen_server call to the service
    {ok, list_to_binary(uuid:to_string(uuid:uuid4()))}.

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