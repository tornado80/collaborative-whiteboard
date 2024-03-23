-module(board_sup).

-behaviour(supervisor).

-export([start_link/1, get_board_service_pid/2]).

-export([init/1]).

start_link(BoardId) ->
    supervisor:start_link(?MODULE, BoardId).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init(BoardId) ->
    boards_manager_service:request_to_be_registered_and_monitored(BoardId, ?MODULE, self()),
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 1},
    ChildSpecs = [#{
                     id => board_database_service,
                     start => {board_database_service, start_link, [BoardId, self()]},
                     restart => permanent,
                     shutdown => 5000,
                     type => worker,
                     modules => [board_database_service]
                 },
                 #{
                     id => board_cache_service,
                     start => {board_cache_service, start_link, [BoardId, self()]},
                     restart => permanent,
                     shutdown => 5000,
                     type => worker,
                     modules => [board_cache_service]
                 },
                 #{
                     id => board_controller_service,
                     start => {board_controller_service, start_link, [BoardId, self()]},
                     restart => permanent,
                     shutdown => 5000,
                     type => worker,
                     modules => [board_controller_service]
                 }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

get_board_service_pid(SupPid, ServiceName) ->
    Entry = lists:keyfind(ServiceName, 1, supervisor:which_children(SupPid)),
    element(2, Entry).