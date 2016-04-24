-module(elevator_scheduler_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    application:ensure_all_started(elevator_scheduler),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() -> 
    [all_sites].

all_sites(_Config) -> 
    ok = es_site_manager:add_site(north),
    ok = es_site_manager:add_site(south),

    ok = es_site_manager:add_node(north),
    ok = es_site_manager:add_node(north),
    ok = es_site_manager:add_node(south),
    ok = es_site_manager:add_node(south),

    1 = node_value(north, 'north-1', floor),
    [] = node_value(south, 'south-2', goal_floors),

    ok = es_site_manager:pickup(north, 5, down),
    ok = es_site_manager:pickup(north, 6, up),
    
    1 = length(node_value(north, 'north-1', goal_floors)),
    1 = length(node_value(north, 'north-2', goal_floors)),
    
    ok = es_site_manager:step(5),

    5 = node_value(north, 'north-1', floor),
    down = node_value(north, 'north-1', direction),
    ok = es_site_manager:step(),
    6 = node_value(north, 'north-2', floor),
    up = node_value(north, 'north-2', direction),
    ok.

node_value(Site, Node, Key) ->
    Status = es_site_manager:status(),
    SiteStatus = proplists:get_value(Site, Status),
    Nodes = proplists:get_value(nodes, SiteStatus),
    NodeStatus = proplists:get_value(Node, Nodes),
    proplists:get_value(Key, NodeStatus).
