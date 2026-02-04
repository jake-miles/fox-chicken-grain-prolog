:- use_module(library(builtins)).
:- use_module(library(lists)).

/*
The fox, chicken and grain puzzle, in prolog using a
depth-first search of the problem space.

There's a farmer in a boat on the shore of a river.
On the riverbank he has three items: a fox, a chicken and a bag of grain.
He needs to get them all across the river.

The fox wants to eat the chicken, so they must always be kept apart.
The chicken wants to eat the grain, so they must always be kept apart.

The farmer is able to hold up to
two of the three items at a time, so wherever he is,
he can keep the fox from eating the chicken and the chicken from eating
the grain, but he can't leave them alone together.

But he needs both hands to row the boat, and he can only fit
one item in the boat with him at a time.

What sequence of moves will let the farmer get the fox, chicken and grain across
the river?
*/

%% In prolog:

items([fox, chicken, grain]).

item(Item) :-
    items(Items),
    member(Item, Items).

shore(near).
shore(far).

location(L) :- shore(L).
location(boat).
location(hands).

can_move(boat).
can_move(X) :- item(X).

predator_prey(fox, chicken).
predator_prey(chicken, grain).

can_hold(boat, Items) :-
    list_length_at_most(Items, 1).

can_hold(hands, Items) :-
    list_length_at_most(Items, 2).

can_hold(Location, Items) :-
    shore(Location),
    list_length_at_most(Items, 3),
    safe_alone(Items).

safe_alone(Items) :-
    maplist(safe_alone_(Items), Items).

safe_alone_(Items, Item) :-
    maplist(\OtherItem^(\+ predator_prey(Item, OtherItem))).

/*
  To express the rest of the problem and its solution,
  we need a concept of the current state of the world.
*/

state_object_location(Pairs, Object, Location) :-
    member(Object-Location, Pairs).

state_location_objects(Pairs, Location, Objects) :-
    findall(Object, state_object_location(Object, Location), Objects).

state_object_newlocation_newstate(Pairs, Object, Loc, [Object-Loc|Others]) :-
    select(Object-_, Pairs, Others).

state_initial(S) :-
    items(Items),
    state_location_objects(S, near, Items).

state_goal(S) :-
    items(Items),
    state_location_objects(S, far, Items).

/*
  The solution is a sequence of legal moves that move us
  from the initial state to the goal state.
*/

solution(Moves) :-
    state_initial(Initial),
    state_goal(Goal),
    moves_from_to(Moves, Initial, Goal).

moves_from_to([], Goal, Goal),
moves_from_to([First|Rest], Initial, Goal) :-
    state_move_newstate(Initial, First, S),
    moves_from_to(Rest, S, Goal).

state_move_newstate(Before, Move, After) :-
    can_move(Object),
    state_can_move(Before, Object, From, To),
    state_object_newlocation(After, Object, To).

state_can_move(S, Item, From, To) :-
    state_object_location(S, Item, From),
    state_location_location_adjacent(S, From, To),
    state_location_objects(S, To, AlreadyThere),
    can_hold(To, [Item|AlreadyThere]).

state_location_location_adjacent(_, boat, hands).
state_location_location_adjacent(_, hands, boat).

state_location_location_adjacent(S, boat, Shore) :-
    state_object_location(S, boat, Shore).

state_location_location_adjacent(S, hands, Shore) :-
    state_location_location_adjacent(S, boat, Shore).
