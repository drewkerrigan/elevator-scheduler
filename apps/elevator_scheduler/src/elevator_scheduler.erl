%%%-------------------------------------------------------------------
%% @doc elevator_scheduler public API
%% @end
%%%-------------------------------------------------------------------

-module(elevator_scheduler).

-behaviour(application).

%% API
-export([start/0,
         start/2, 
         stop/1]).

-export([status/0,
         update/3,
         pickup/2,
         step/0]).

%%====================================================================
%% API
%%====================================================================

start() ->
    es_sup:start_link().

start(_StartType, _StartArgs) ->
    es_sup:start_link().

stop(_State) ->
    ok.

%%--------------------------------------------------------------------

-spec status() -> [{integer(), integer(), integer()}].
status() ->
    [{1,1,1}].

-spec update(integer(), integer(), integer()) -> {error, term()} | ok.
update(_A, _B, _C) ->
    ok.

-spec pickup(integer(), integer()) -> {error, term()} | ok.
pickup(_A, _B) ->
    ok.

-spec step() -> {error, term()} | ok.
step() ->
    ok.


%%====================================================================
%% Internal functions
%%====================================================================
