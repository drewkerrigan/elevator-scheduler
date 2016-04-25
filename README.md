# Elevator Scheduler

An OTP application to control and schedule multiple groups (`sites`) of multiple elevators (`nodes`).

## Precompiled Tarball

A Mac OSX 10.11 precompiled tarball is available here: []().

## Build

    $ make rel
    
## Run

    $ make rel
    
## Test

    $ make test
    
## Create Tarball
    
    $ make tar
    
## Implementation

Following is the supervision tree for this application:

    -elevator_scheduler (app)
        |
        -es_sup (supervisor)
            |
            -es_site_manager (supervisor)
            |   |
            |   -es_site_fsm (gen_fsm)
            |
            -es_node_manager (supervisor)
                |
                -es_node_fsm (gen_fsm)
                
Each time a site is added, a `es_site_fsm` process is created and supervised by `es_site_manager`. Similarly,
each time a `node` is added to a `site`, a `es_node_fsm` process is created and supervised by `es_node_manager`, 
initiated by a call within `es_site_fsm`.

This structure makes it possible to reason about how to allocate `nodes` to pickup requests when there is more 
than one node available.

### Scheduling
    
When a pickup is requested at a `site`, each of the `nodes` (elevators) are 
queried to determine which would provide the shortest trip given a few circumstances such as:

* Elevator's current floor
* Destination floor
* Where that stop lands in a simulated recalculation of that `node`'s current `goal_floor` queue

Here's an inspection and analysis of a few situations:

```
es_site_manager:add_site(north).
es_site_manager:add_node(north).
es_site_manager:add_node(north).
es_site_manager:status().
[{north,[{key,north},
         {state,ready},
         {nodes,[{'north-1',[{key,'north-1'},
                             {state,ready},
                             {floor,1},
                             {goal_floors,[]},
                             {direction,up}]},
                 {'north-2',[{key,'north-2'},
                             {state,ready},
                             {floor,1},
                             {goal_floors,[]},
                             {direction,up}]}]}]}]
```

This is the base status after creating 2 `nodes` in a `site` called `north`.

```
es_site_manager:pickup(north, 5, up).
es_site_manager:pickup(north, 5, down).
es_site_manager:status().
[{north,[{key,north},
         {state,ready},
         {nodes,[{'north-1',[{key,'north-1'},
                             {state,moving},
                             {floor,1},
                             {goal_floors,[{5,up}]},
                             {direction,up}]},
                 {'north-2',[{key,'north-2'},
                             {state,moving},
                             {floor,1},
                             {goal_floors,[{5,down}]},
                             {direction,up}]}]}]}]
```

You can see above that the 2 pickup requests were spead evenly across the elevators at first.
This is due to an attempt to utilize all available elevators before scheduling additional tasks on busy ones.

```
es_site_manager:step(5).
es_site_manager:status().
[{north,[{key,north},
         {state,ready},
         {nodes,[{'north-1',[{key,'north-1'},
                             {state,moving},
                             {floor,5},
                             {goal_floors,[]},
                             {direction,up}]},
                 {'north-2',[{key,'north-2'},
                             {state,moving},
                             {floor,5},
                             {goal_floors,[]},
                             {direction,down}]}]}]}]
```

We've stepped 5 ticks in our simulator, and are now on the respective goal floors.

```
es_site_manager:pickup(north, 10, down).
es_site_manager:pickup(north, 3, up).
es_site_manager:pickup(north, 13, down).
es_site_manager:pickup(north, 6, up).
es_site_manager:status().
[{north,[{key,north},
         {state,ready},
         {nodes,[{'north-1',[{key,'north-1'},
                             {state,moving},
                             {floor,5},
                             {goal_floors,[{6,up},{13,down},{10,down}]},
                             {direction,up}]},
                 {'north-2',[{key,'north-2'},
                             {state,moving},
                             {floor,5},
                             {goal_floors,[{3,up}]},
                             {direction,down}]}]}]}]
```

Now we can see the individual node scheduling in action: 

* `north-1` was selected for 10, down.
* `north-2` was selected for 3, up because the calculated number of steps for `north-1` to get there was too high.
* `north-1` was also selected for 13, down and 6, up as well, because an efficient ordering of the floors determined that
it was the most suitable.

Note that the order of `north-1`'s new queue after every floor is requested: It goes all the way to the top before returning back down to service more requests.

```
es_site_manager:step(15).
es_site_manager:status().
[{north,[{key,north},
         {state,ready},
         {nodes,[{'north-1',[{key,'north-1'},
                             {state,ready},
                             {floor,10},
                             {goal_floors,[]},
                             {direction,down}]},
                 {'north-2',[{key,'north-2'},
                             {state,ready},
                             {floor,3},
                             {goal_floors,[]},
                             {direction,up}]}]}]}]
```

Given enough steps, all floors will drain their `goal_floors` queues, and return to ready state.
