%%%-------------------------------------------------------------------
%% @doc elevator_scheduler top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(es_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SiteManagerSpec = {es_site_manager,
                              {es_site_manager, start_link, []},
                              permanent, 5000, supervisor,
                              [es_site_manager]},
    NodeManagerSpec = {es_node_manager,
                           {es_node_manager, start_link, []},
                           permanent, 5000, supervisor,
                           [es_node_manager]},
    Specs = [SiteManagerSpec, NodeManagerSpec],
    {ok, { {one_for_one, 1, 1}, Specs} }.

%%====================================================================
%% Internal functions
%%====================================================================
