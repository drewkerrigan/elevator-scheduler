-module(es_node_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/2]).

-export([pickup/3,
         step/1,
         status/1]).

%% gen_fsm callbacks
-export([ready/2, ready/3,
         moving/2, moving/3]).

-export([init/1, 
         handle_event/3,
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {
          key :: atom(),
          site_key :: atom(),
          floor = 1 :: non_neg_integer(),
          goal_floors = [] :: [{non_neg_integer(), up | down}],
          direction = up :: up | down
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

-spec start_link(atom(), atom()) -> {ok, pid()} | {error, term()}.
start_link(Key, SiteKey) ->
    gen_fsm:start_link(?MODULE, {Key, SiteKey}, []).

-spec pickup(pid(), non_neg_integer(), up | down) -> ok.
pickup(Pid, Floor, Direction) ->
    gen_fsm:send_all_state_event(Pid, {pickup, Floor, Direction}).

-spec step(pid()) -> ok.
step(Pid) ->
    gen_fsm:send_event(Pid, step).

-spec status(pid()) -> ok.
status(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, status).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

-spec init(atom()) -> {ok, state_name(), #state{}}.
init({Key, SiteKey}) ->
    {ok, ready, #state{key = Key,
                       site_key = SiteKey}}.

-spec ready(term(), #state{}) -> async_reply().
ready(step, State) ->
    {next_state, ready, State};
ready(_Event, State) ->
    {stop, {error, unhandled_event}, State}.

-spec moving(term(), #state{}) -> async_reply().
moving(step, State = 
           #state{goal_floors = []}) ->
    {next_state, ready, State};
moving(step, State = 
           #state{floor = Floor,
                  goal_floors = 
                      [{GoalFloor, GoalDirection}|Rest]}) ->
    case es_util:direction(Floor, GoalFloor) of
        equal ->
            {next_state, moving, 
             State#state{goal_floors = Rest,
                         direction = GoalDirection}};
        up ->
            {next_state, moving, 
             State#state{floor = Floor + 1,
                         direction = up}};
        down ->
            {next_state, moving, 
             State#state{floor = Floor - 1,
                         direction = down}}
    end;
moving(_Event, State) ->
    {stop, {error, unhandled_event}, State}.

-spec ready(term(), pid(), #state{}) -> sync_reply().
ready(_Event, _From, State) ->
    {reply, {error, unhandled_event}, ready, State}.

-spec moving(term(), pid(), #state{}) -> sync_reply().
moving(_Event, _From, State) ->
    {reply, {error, unhandled_event}, ready, State}.

-spec handle_event(term(), state_name(), #state{}) -> async_reply().
handle_event({pickup, Floor, Direction}, _StateName, State =
                 #state{floor = CurrentFloor,
                        direction = CurrentDirection,
                        goal_floors = GoalFloors}) ->
    GoalFloors1 = [{Floor, Direction} | GoalFloors],
    GoalFloors2 = sort_goal_floors(CurrentFloor, CurrentDirection, GoalFloors1),
    State1 = State#state{goal_floors = GoalFloors2},
    {next_state, moving, State1};
handle_event(_Event, _StateName, State) ->
    {stop, {error, unhandled_event}, State}.

-spec handle_sync_event(term(), pid(), state_name(), #state{}) ->
                               sync_reply().
handle_sync_event(status, _From, StateName, State = 
                 #state{key = Key,
                        floor = Floor,
                        goal_floors = GoalFloor,
                        direction = Direction}) ->
    Reply = [{key, Key},
             {state, StateName},
             {floor, Floor},
             {goal_floors, GoalFloor},
             {direction, Direction}],
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

-spec sort_goal_floors(non_neg_integer(), up | down,
                      [{non_neg_integer(), up | down}]) -> 
                             [{non_neg_integer(), up | down}].
sort_goal_floors(F1, D1, GoalFloors) ->
    Sorted1 = 
        lists:keysort(1, [ {es_util:distance(F1, F2), {F2, D2}} 
                           || {F2, D2} <- GoalFloors, 
                              D2 =:= D1 ]),
    Sorted2 = 
        lists:keysort(1, [ {es_util:distance(F1, F2), {F2, D2}} 
                           || {F2, D2} <- GoalFloors, 
                              D2 =:= es_util:invert_direction(D1) ]),
    [{F2, D2} || {_, {F2, D2}} <- Sorted1 ++ Sorted2 ].
