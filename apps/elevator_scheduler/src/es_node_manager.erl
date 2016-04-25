%%%-------------------------------------------------------------------
%%% @author Drew Kerrigan <dkerrigan@Andrews-MacBook-Pro-2.local>
%%% @copyright (C) 2016, Drew Kerrigan
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2016 by Drew Kerrigan <dkerrigan@Andrews-MacBook-Pro-2.local>
%%%-------------------------------------------------------------------
-module(es_node_manager).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([add_node/2,
         update_node/3,
         update_node/4,
         get_nodes/0,
         pickup/3,
         step/1,
         status/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {error, term()} | {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec add_node(atom(), atom()) -> ok | {error, term()}.
add_node(Key, SiteKey) ->
    Spec = {Key,
        {es_node_fsm, start_link, [Key, SiteKey]},
        transient, 5000, worker, [es_node_fsm]},
    case supervisor:start_child(?MODULE, Spec) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _Pid}} ->
            {error, exists};
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_node(atom(), non_neg_integer(), up | down) -> {error, term()} | ok.
update_node(Key, Floor, Direction) ->
    case get_node_pid(Key) of
        {error, Reason} ->
            {error, Reason};
        Pid ->
            es_node_fsm:update(Pid, Floor, Direction)
    end.

-spec update_node(atom(), non_neg_integer(), up | down, [term()]) -> {error, term()} | ok.
update_node(Key, Floor, Direction, GoalFloors) ->
    case get_node_pid(Key) of
        {error, Reason} ->
            {error, Reason};
        Pid ->
            es_node_fsm:update(Pid, Floor, Direction, GoalFloors)
    end.

-spec get_nodes() -> [{atom(), pid()}].
get_nodes() ->
    [ {Key, Pid} || {Key, Pid, _, _} <- 
                        supervisor:which_children(?MODULE)].

-spec pickup(atom(), non_neg_integer(), up | down) -> ok.
pickup(Key, Floor, Direction) ->
    case get_node_pid(Key) of
        {error, _} ->
            ok;
        Pid ->
            es_node_fsm:pickup(Pid, Floor, Direction)
    end.

-spec step(atom()) -> ok.
step(Key) ->
    case get_node_pid(Key) of
        {error, _} ->
            ok;
        Pid ->
            es_node_fsm:step(Pid)
    end.

-spec status(atom()) -> {atom(), term()}.
status(Key) ->
    case get_node_pid(Key) of
        {error, _} ->
            ok;
        Pid ->
            {Key, es_node_fsm:status(Pid)}
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init([term()]) -> {error, term()} | {ok, term()}.
init([]) ->
    {ok, {{one_for_one, 1, 1}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_node_pid(atom()) -> pid().
get_node_pid(Key) ->
    proplists:get_value(Key, get_nodes(), {error, not_found}).
