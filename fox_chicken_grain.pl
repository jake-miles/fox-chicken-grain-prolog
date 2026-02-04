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

items([fox, chicken, grain]).

% Something is an item if it's in the above list of items.
item(Item) :-
    items(Items),
    member(Item, Items).

% The river has two shores that we can toggle between.
shore(near).
shore(far).

%% The four locations where the farmer can set items down.
location(L) :- shore(L).
location(boat).
location(hands).

%% The things that can move in this world.
movable(boat).
movable(X) :- item(X).

%% What eats what.
predator_prey(fox, chicken).
predator_prey(chicken, grain).

%% What collection of items can coexist in each location.
location_items_ok(boat, Items) :-
    list_length_at_most(Items, 1).

location_items_ok(hands, Items) :-
    list_length_at_most(Items, 2).

location_items_ok(Location, Items) :-
    shore(Location),
    list_length_at_most(Items, 3),
    safe_alone(Items).

%% Items that can be safely left alone together.
safe_alone(Items) :-
    maplist(safe_alone_(Items), Items).

safe_alone_(Items, Item) :-
    maplist(\OtherItem^(\+ predator_prey(Item, OtherItem))).

/*
  To express the rest of the problem and its solution,
  which involves changing the state of the world,
  we need a concept of the current state of the world.
  These accessors encapsulate that state - a list of Object-Location
  pairs. (The hyphen is the prolog convention for representing pairs
  but isn't special syntax - you can define your own "opearators",
  which just become part of the pattern prolog matches against.
*/

%% Holds if in state S, Object is at Location.
state_object_location(S, Object, Location) :-
    member(Object-Location, S).

%% Holds if in state S, Objects are all t Location.
state_location_objects(S, Location, Objects) :-
    findall(Object, state_object_location(Object, Location), Objects).

%% Holds if, given an initial state, object and new location,
%% newstate is the iniitial state with Object at Location.
state_object_newlocation_newstate(S, Object, Loc, [Object-Loc|Others]) :-
    select(Object-_, S, Others).

%% Holds if S is the initial puzzle state.
state_initial(S) :-
    items(Items),
    state_location_objects(S, near, Items).

%% Holds if S is the goal state of the puzzle.
state_goal(S) :-
    items(Items),
    state_location_objects(S, far, Items).

/*
  So how do we get from the initial state to the goal state?
  The solution is a sequence of legal moves that transform
  the initial state stepwise into the goal state.
*/

solution(Moves) :-
    state_initial(Initial),
    state_goal(Goal),
    moves_from_to(Moves, Initial, Goal).

%% moves_from_to(Moves, Initial, Goal).
%% Holds if Moves is a sequence of legal moves that
%% transform the Initial state into the Goal state.
moves_from_to([], Goal, Goal),
moves_from_to([First|Rest], Initial, Goal) :-
    state_move_newstate(Initial, First, S),
    moves_from_to(Rest, S, Goal).

%% Holds if Move is a valid move from state Before to state After.
state_move_newstate(Before, Move, After) :-
    movable(Object),
    state_can_move(Before, Object, From, To),
    state_object_newlocation(After, Object, To).

%% Holds if in state S, Item can legally move from location From to location To.
state_can_move(S, Item, From, To) :-

    %% the item must be at location From
    state_object_location(S, Item, From),

    %% From and To must be next to each other
    state_location_location_reachable(S, From, To),

    %% The items already in location To
    state_location_items(S, To, AlreadyThere),

    %% We can legallly add Item to that list.
    location_items_ok(To, [Item|AlreadyThere]).

%% state_location_adjacent(S, L1, L2).
%% Holds if in state S, locationL1 is next to state L2,
%% i.e. that an object can move from L1 to L2.

state_location_location_reachable(_, boat, hands).
state_location_location_reachable(_, hands, boat).

state_location_location_reachable(S, boat, Shore) :-
    state_object_location(S, boat, Shore).

state_location_location_reachable(S, hands, Shore) :-
    state_location_location_reachable(S, boat, Shore).

state_location_location_reachable(S, Shore1, Shore2) :-
    shore(Shore1),
    shore(Shore2),
    %% Disallow a move from a shore to itself or we'll could be here all day.
    Shore1 \== Shore2.