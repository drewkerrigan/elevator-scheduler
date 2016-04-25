-module(es_site_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/1]).

-export([add_node/1,
         step/1,
         status/1,
         pickup/3]).

%% gen_fsm callbacks
-export([ready/2, ready/3]).

-export([init/1, 
         handle_event/3,
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {
          key :: atom(),
          node_keys = [] :: [atom()] | undefined
         }).

-type(state_name() :: atom()).

-type(async_reply() ::
        {next_state, state_name(), #state{}} |
        {next_state, state_name(), #state{}, timeout()} |
        {stop, term(), #state{}}).

-type(sync_reply() ::
        {next_state, state_name(), #state{}} |
        {next_state, state_name(), #state{}, timeout()} |
        {reply, term(), state_name(), #state{}} |
        {reply, term(), state_name(), #state{}, timeout()} |
        {stop, term(), #state{}} |
        {stop, term(), term(), #state{}}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
start_link(Key) ->
    gen_fsm:start_link(?MODULE, Key, []).

-spec add_node(pid()) -> ok | {error, term()}.
add_node(Pid) ->
    gen_fsm:sync_send_event(Pid, add_node).

-spec pickup(pid(), non_neg_integer(), up | down) -> {error, term()} | ok.
pickup(Pid, Floor, Direction) ->
    gen_fsm:sync_send_event(Pid, {pickup, Floor, Direction}).

-spec step(pid()) -> ok.
step(Pid) ->
    gen_fsm:send_event(Pid, step),
    ok.

-spec status(pid()) -> [{atom(), term()}].
status(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, status).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

-spec init(atom()) -> {ok, state_name(), #state{}}.
init(Key) ->
    {ok, ready, #state{key = Key}}.

-spec ready(term(), #state{}) -> async_reply().
ready(step, State = #state{node_keys = NodeKeys}) ->
    [ es_node_manager:step(NodeKey) || NodeKey <- NodeKeys ],
    {next_state, ready, State};
ready(_Event, State) ->
    {stop, {error, unhandled_event}, State}.

-spec ready(term(), pid(), #state{}) -> sync_reply().
ready(add_node, _From, State = 
          #state{key = Key,
                 node_keys = NodeKeys}) ->
    NodeNum = length(NodeKeys) + 1,
    NodeKey = list_to_atom(atom_to_list(Key) ++ "-" ++ 
                           integer_to_list(NodeNum)),
    case es_node_manager:add_node(NodeKey, Key) of
        {error, Reason} ->
            {reply, {error, Reason}, ready, State};
        ok ->
            NodeKeys1 = [NodeKey | NodeKeys],
            State1 = State#state{node_keys = lists:reverse(NodeKeys1)},
            {reply, ok, ready, State1}
    end;
ready({pickup, Floor, Direction}, _From, State = 
          #state{node_keys = NodeKeys}) ->
    case length(NodeKeys) of
        0 -> 
            {reply, {error, no_nodes}, ready, State};
        _ ->
            NodeStatuses = 
                [ es_node_manager:status(K)
                  || K <- NodeKeys ],
            Reply = do_pickup(Floor, Direction, NodeStatuses),
            {reply, Reply, ready, State}
    end;
ready(_Event, _From, State) ->
    {reply, {error, unhandled_event}, ready, State}.

-spec handle_event(term(), state_name(), #state{}) -> async_reply().
handle_event(_Event, _StateName, State) ->
    {stop, {error, unhandled_event}, State}.

-spec handle_sync_event(term(), pid(), state_name(), #state{}) ->
                               sync_reply().
handle_sync_event(status, _From, StateName, State = 
                      #state{key = Key,
                             node_keys = NodeKeys}) ->
    Reply = [{key, Key},
             {state, StateName},
             {nodes, [ es_node_manager:status(N) || N <- NodeKeys ]}],
    {reply, Reply, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, unhandled_event}, StateName, State}.

-spec handle_info(term(), state_name(), #state{}) -> async_reply().
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

-spec terminate(term(), state_name(), #state{}) -> ok.
terminate(_Reason, _StateName, _State) ->
    ok.

-spec code_change(term(), state_name(), #state{}, term()) ->
                         {ok, state_name(), #state{}}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec do_pickup([{atom(), term()}], non_neg_integer(), up | down) -> ok.
do_pickup(Floor, Direction, NodeStatuses) ->
    [TargetNode|_] = sort_node_distances(Floor, Direction, NodeStatuses),
    es_node_manager:pickup(TargetNode, Floor, Direction).

sort_node_distances(Floor, Direction, NodeStatuses) ->
    Sorted1 = filter_sort_node_distances(Floor, NodeStatuses, Direction),
    Sorted2 = filter_sort_node_distances(Floor, NodeStatuses, 
                                         es_util:invert_direction(Direction)),
    Sorted1 ++ Sorted2.

filter_sort_node_distances(Floor, NodeStatuses, FilterDirection) ->
    Nodes = [ {es_util:distance(Floor, calc_node_goal_floor(Status)), K}
      || {K, Status} <- NodeStatuses,
         FilterDirection =:= calc_node_goal_direction(Floor, Status)
    ],
    Sorted = lists:keysort(1, Nodes),
    [ N || {_, N} <- Sorted ].

calc_node_goal_floor(Status) ->
    case proplists:get_value(goal_floors, Status) of
        [] ->
            proplists:get_value(floor, Status);
        [{GF, _}|_] ->
            GF
    end.

calc_node_goal_direction(F2, Status) ->
    case proplists:get_value(goal_floors, Status) of
        [] ->
            F1 = calc_node_goal_floor(Status),
            es_util:direction(F1, F2);
        [{_, GD}|_] ->
            GD
    end.
