-module(elevator_scheduler_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    application:ensure_all_started(elevator_scheduler),
    ok = es_site_manager:add_site(north),
    ok = es_site_manager:add_site(south),
    ok = es_site_manager:add_node(north),
    ok = es_site_manager:add_node(north),
    ok = es_site_manager:add_node(south),
    1 = node_value(north, 'north-2', floor),
    [] = node_value(south, 'south-1', goal_floors),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    reset_sites(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() -> 
    [one_node,
     two_nodes].

one_node(_Config) -> 
    ok = es_site_manager:pickup(south, 5, down),
    1 = length(node_value(south, 'south-1', goal_floors)),
    ok = es_site_manager:step(5),
    5 = node_value(south, 'south-1', floor),
    down = node_value(south, 'south-1', direction),
    ok = es_site_manager:pickup(south, 1, up),
    ok = es_site_manager:step(3),
    2 = node_value(south, 'south-1', floor),
    down = node_value(south, 'south-1', direction),
    ok = es_site_manager:pickup(south, 10, up),
    ok = es_site_manager:step(11),
    10 = node_value(south, 'south-1', floor),
    up = node_value(south, 'south-1', direction),
    ok.

two_nodes(_Config) ->
    ok = es_site_manager:pickup(north, 11, up),
    ok = es_site_manager:pickup(north, 10, down),
    ok = es_site_manager:pickup(north, 5, down),
    ok = es_site_manager:pickup(north, 6, up),
    2 = length(node_value(north, 'north-1', goal_floors)),
    2 = length(node_value(north, 'north-2', goal_floors)),
    ok = es_site_manager:step(5),
    6 = node_value(north, 'north-1', floor),
    up = node_value(north, 'north-1', direction),
    6 = node_value(north, 'north-2', floor),
    up = node_value(north, 'north-2', direction),
    ok = es_site_manager:step(),
    6 = node_value(north, 'north-1', floor),
    up = node_value(north, 'north-1', direction),
    7 = node_value(north, 'north-2', floor),
    up = node_value(north, 'north-2', direction),
    17 = es_node_manager:steps_to_pickup('north-1', 1, up),
    15 = es_node_manager:steps_to_pickup('north-2', 1, up),
    ok = es_site_manager:pickup(north, 1, up),
    ok = es_site_manager:step(5),
    11 = node_value(north, 'north-1', floor),
    up = node_value(north, 'north-1', direction),
    9 = node_value(north, 'north-2', floor),
    down = node_value(north, 'north-2', direction).

node_value(Site, Node, Key) ->
    Status = es_site_manager:status(),
    SiteStatus = proplists:get_value(Site, Status),
    Nodes = proplists:get_value(nodes, SiteStatus),
    NodeStatus = proplists:get_value(Node, Nodes),
    proplists:get_value(Key, NodeStatus).

reset_sites() ->
    ok = es_node_manager:update_node('north-1', 1, up, []),
    ok = es_node_manager:update_node('north-2', 1, up, []),
    ok = es_node_manager:update_node('south-1', 1, up, []).
