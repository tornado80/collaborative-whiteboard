%%%-------------------------------------------------------------------
%% @doc backend top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(board_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link(?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 10,
                 period => 1},
    ChildSpecs = [#{
                     id => board_database_service,
                     start => {board_database_service, start_link, []},
                     restart => permanent,
                     shutdown => 5000,
                     type => worker,
                     modules => [board_database_service]
                 },
                 #{
                     id => board_cache_service,
                     start => {board_cache_service, start_link, []},
                     restart => permanent,
                     shutdown => 5000,
                     type => worker,
                     modules => [board_cache_service]
                 },
                 #{
                     id => board_manager_service,
                     start => {board_manager_service, start_link, []},
                     restart => permanent,
                     shutdown => 5000,
                     type => worker,
                     modules => [board_manager_service]
                 }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions