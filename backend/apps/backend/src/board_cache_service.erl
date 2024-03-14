-module(board_cache_service).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions
-export([create_blob/2, try_get_blob/2]).

%% State record
-record(state, {}).

%% API functions
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec create_blob(pid(), binary()) -> {ok, binary()}.
create_blob(Pid, Body) ->
    gen_server:call(Pid, {create_blob, Body}).

-spec try_get_blob(pid(), binary()) -> {ok, binary()} | notfound.
try_get_blob(Pid, BlobId) ->
    gen_server:call(Pid, {try_get_blob, BlobId}).

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