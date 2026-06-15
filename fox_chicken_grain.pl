/*
  The fox, chicken and grain puzzle, in prolog.

  There's a farmer in a boat on the shore of a river.
  On the riverbank he has three items: a fox, a chicken and a bag of grain.
  He needs to get them all across the river.

  The fox wants to eat the chicken, so they can't be left alone together.
  The chicken wants to eat the grain, so they can't be left alone together.

  Only one item at a time will fit in the boat with the farmer to
  cross the river, and he needs both hands free to row the boat across
  the river.

  How can the farmer get the fox, chicken and grain across the river?
  */

:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(yall).


% first, let's meet prolog by defining a couple list operations we'll use.

member(X, [X|_]).
member(X, [_|T]) :-
    member(X, T).

pairs_key_value(Pairs, Key, Value) :-
    member(Key-Value, Pairs).

pairs_key_value_updated(Pairs, Key, Value, [Key-Value|Others]) :-
    exclude([Key-_], Pairs, Others).

pairs_value_keys(Pairs, Value, Keys) :-
    findall(Key, pairs_key_value(Pairs, Key, Value), Keys).


% model the problem.

% three items
items([fox, chicken, grain]).

% four total objects we can move around
objects([boat|Items]) :-
    items(Items).

% what eats what.
predator_prey(fox, chicken).
predator_prey(chicken, grain).

state_initial(S) :- state_location_has_all_objects(S, near).
state_desired(S) :- state_location_has_all_objects(S, far).

state_location_has_all_objects(S, Location) :-
    objects(Objects),
    state_location_objects(S, Location, Objects).

% We're going to solve the problem by moving things around one at a time.
% So we need to model moves: how the different objects can move,
% which changes depending on the current state of the world.

% So we need to represent the state of the world,
% which we will transform step by step to achieve the goal.
% The world will be a list of Object-Location pairs,
% denoting where each object is in that state.

% Define the data representaion and encapsulate it by defining accessors:

state_object_location(S, Object, Location) :-
    pairs_key_value(S, Object, Location).

state_location_objects(S, Location, Objects) :-
    pairs_value_keys(S, Location, Objects).

% represent a move with a term `object_to(Object, Location)`
state_move_applied(S, object_to(Object, Location), Updated) :-
    pairs_key_value_updated(S, Object, Location, Updated).

% What is the definition of a solution to the puzzle?

solution(Moves) :-
  state_initial(Before),
  state_desired(After),
  moves_before_after(Moves, Before, After).

% if before and after are the same state, the list of required moves is empty.
moves_before_after([], State, State).

% otherwise, there's some first move, `First` that transforms state `Start`
% into some new state `Start1`, and a list of subsequent moves, `Rest`,
% that transform `Start1` into `Goal`.
moves_before_after([First|Rest], Start, Goal) :-
  move_before_after(First, Start, Start1),
  moves_before_after(Rest, Start1, Goal).

% Represent a move with a structure `object_to(Object, Location)`,
% relating an object to the location it moves to.

move_before_after(Move, Before, After) :-
    state_move_error(Before, Move, none),
    state_move_applied(Before, Move, After).

% two types of moves: moving the boat, and moving an item.

% moving the boat to the other shore
state_move_error(S, object_to(boat, OtherShore), Error) :-
    state_object_location(S, boat, Shore),
    other_shore(OtherShore, Shore),
    validate(state_hands_empty(S), "Farmer's hands are not empty", Error).

% moving an item
state_move_error(S, object_to(Item, To), Error) :-
    item(Item),
    state_object_location(S, Item, From),
    state_locations_adjacent(S, From, To),
    state_location_objects(S, To, ItemsAlreadyThere),
    location_items_ok(To, [Item|ItemsAlreadyThere], Error).

validate(Goal, Message, Error) :-
    if_(call(Goal),
        Error = none,
        Error = Message).

item(Item) :-
    items(Items),
    member(Item, Items).

other_shore(X, Y) :-
    shore(X),
    shore(Y),
    X \== Y.

shore(near).
shore(far).

state_hands_empty(S) :-
    state_location_objects(S, hands, []).

state_locations_adjacent(_, boat, hands).

state_locations_adjacent(S, hands, Shore) :-
    state_object_location(S, boat, Shore).

location_items_ok(Location, Items, Error) :-
    location_items_fit(Location, Items, Error),
    items_safe_together(Items).

location_items_fit(Location, Items, Error) :-
    location_capacity(Location, C),
    length(Items, L),
    validate(C #> L, "", Error).

location_capacity(Location, Capacity) :-
    location_capacities(LCs),
    member([Location, Capacity], LCs).

location_capacities([near-3, far-3, boat-1, hands-2]).

items_safe_together(Items) :-
    list_cartesian_pairs(Items, Pairs),
    maplist(item_item_safe_together, Pairs).

item_item_safe_together(X, Y) :-
    \+ predator_prey(X, Y),
    \+ predator_prey(Y, X).

list_cartesian_pairs(Xs, Product) :-
    maplist(maplist(X,Y >> [X, Y], Xs), Xs, Product).
