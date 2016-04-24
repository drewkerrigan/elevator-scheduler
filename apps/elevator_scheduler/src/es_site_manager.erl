-module(es_site_manager).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([add_site/1,
         get_sites/0,
         add_node/1,
         pickup/3,
         step/0, step/1,
         status/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {error, term()} | {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec add_site(atom()) -> ok | {error, term()}.
add_site(Key) ->
    Spec = {Key,
        {es_site_fsm, start_link, [Key]},
        transient, 5000, worker, [es_site_fsm]},
    case supervisor:start_child(?MODULE, Spec) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _Pid}} ->
            {error, exists};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_sites() -> [{atom(), pid()}].
get_sites() ->
    Sites = [ {Key, Pid} || {Key, Pid, _, _} <- 
                                supervisor:which_children(?MODULE) ],
    lists:reverse(Sites).

-spec add_node(atom()) -> {error, term()} | ok.
add_node(Key) ->
    case get_site_pid(Key) of
        {error, Reason} ->
            {error, Reason};
        Pid ->
            es_site_fsm:add_node(Pid)
    end.

-spec pickup(atom(), non_neg_integer(), up | down) -> {error, term()} | ok.
pickup(Key, Floor, Direction) ->
    case get_site_pid(Key) of
        {error, Reason} ->
            {error, Reason};
        Pid ->
            es_site_fsm:pickup(Pid, Floor, Direction)
    end.

-spec step(non_neg_integer()) -> ok.
step(0) ->
    ok;
step(Times) ->
    step(),
    step(Times -1).

-spec step() -> ok.
step() ->
    [ es_site_fsm:step(Pid) || {_, Pid} <- get_sites() ],
    ok.

-spec status() -> [term()].
status() ->
    [ {Key, es_site_fsm:status(Pid)} || {Key, Pid} <- get_sites() ].

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init([term()]) -> {error, term()} | {ok, term()}.
init([]) ->
    {ok, {{one_for_one, 1, 1}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_site_pid(atom()) -> pid().
get_site_pid(Key) ->
    proplists:get_value(Key, get_sites(), {error, not_found}).
