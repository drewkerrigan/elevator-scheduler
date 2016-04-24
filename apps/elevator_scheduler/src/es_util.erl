-module(es_util).

-export([direction/2,
         distance/2,
         invert_direction/1]).

direction(A, A) -> equal;
direction(A, B) when A > B -> down;
direction(A, B) when B > A -> up.

distance(A, A)-> 0;
distance(A, B) when A > B -> A - B;
distance(A, B) when B > A -> B - A.

invert_direction(up) -> down;
invert_direction(down) -> up.
