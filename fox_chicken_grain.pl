:- use_module(library(apply)).
:- use_module(library(clpfd)).

/*
  The fox, chicken and grain puzzle, in prolog.

  A farmer has three items: a fox, a chicken and a bag of grain, on
  one shore of a river. He needs to get them all across the river. He
  has a row boat, and needs both hands to row it. He can only fit one
  item in the boat with him at a time, and he can only hold one item
  in each hand at a time.

  The fox wants to eat the chicken, so they can't be left alone together.
  The chicken wants to eat the grain, so they can't be left alone together.

  How can the farmer get the fox, chicken and grain across the river?
  */

location(near).
location(far).
location(boat).
location(farmer).

%?- location(boat).

%?- location(house).

%?- location(X).

%?- findall(X, location(X), Locations).

% what eats what.
predator_prey(fox, chicken).
predator_prey(chicken, grain).

% ?- predator_prey(X, Y).

% We'll represent the world as a list of Object-Location pairs.

% The prolog convention is to represent a pair with a hyphen.
% our world state is a list of Object-Location pairs, denoting
% where each object is in this state of the world.

% The state of the world when we start the puzzle
state_start([fox-near, chicken-near, grain-near, boat-near]).

% and how we want it to look in the end:
state_final([fox-far, chicken-far, grain-far, boat-far]).

%?- state_start(S).

%?- state_start([fox-F, chicken-C, grain-G, boat-B]).

%?- state_start([_, _, G, _]).

%?- state_start([_, _, grain-G, _]).

%?- state_start([fox-F|_]).

%?- state_start([_, chicken-C|_]).

% ?- state_start(S), state_state_equal(S, S).

% ?- state_state_equal([chicken-near, fox-near, grain-near, boat-near],
%                      [fox-near, chicken-near, grain-near, boat-near]).

%%%% web page rendering the state

% but we want to access the members of the list more generally.
% `member` is built-in, but let's define it to see how it works.

member(X, [X|_]).

member(X, [_|T]) :-
    member(X, T).

%?- member(a, [a,b,c]).

%?- member(a, [1,2,3]).

%?- member(X, [a,b,c]).

%?- member(b, [a, X, c]).

%?- findall(X, member(X, [a,b,c]), Xs).

%?- member(X, L).

% and we can use `member` to more access our list of Object-Location pairs
% more generally.

%?- state_start(S), member(Object-Location, S).

%?- state_start(S), findall(Object-Location, member(Object-Location, S), Pairs).

%?- state_start(S), findall(Object, member(Object-_, S), Objects).

%?- state_start(S), findall(Location, member(_-Location, S), Locations).

% Our list is a list of pairs. let's define some more specific
% accessors for a list of key-value pairs:

pairs_key_value(Pairs, Key, Value) :-
    member(Key-Value, Pairs).

%?- pairs_key_value([a-1, b-2, c-3], a, Value).

 %?- pairs_key_value([a-1, b-2, c-3], Key, 5).

%?- pairs_key_value([a-1, b-2, c-1], Key, 1).

% Keys are the keys in Pairs with value Value.
pairs_value_keys(Pairs, Value, Keys) :-
    findall(Key, pairs_key_value(Pairs, Key, Value), Keys).

%?- pairs_value_keys([a-1, b-2, c-1], 1, Keys).

% Holds if Key is the key in the given pair.
key_of_pair(Key, Key-_).

%?- key_of_pair(Key, a-3).

%?- key_of_pair(a, a-3).

%?- key_of_pair(a, b-3).

%?- key_of_pair(K, K-V).

%?- key_of_pair(K, X-V).

value_of_pair(_-Value, Value).

% to "update" a key's value in a list of pairs,
% produce a new list with that key-value, and excluding that
% key's original pair from the "before" list.
% Note that this provides a partially-applied predicate, key_of_pair(Key),
% to the higher-order predicate `exclude`.
pairs_key_value_updated(Pairs, Key, Value, [Key-Value|Others]) :-
    exclude(key_of_pair(Key), Pairs, Others).

%?- pairs_key_value_updated([a-1, b-2, c-3], b, 4, Updated).

%?- pairs_key_value_updated([a-1, b-2, c-3], b, 4, [b-4, a-1, c-3]).

%?- pairs_key_value_updated([a-1, b-2, c-3], Key, Value, [b-4, a-1, c-3]).

% now we have what we need to define accessors for our list of Object-Location pairs.

state_object_location(S, Object, Location) :-
    pairs_key_value(S, Object, Location).

%?- state_start(S), state_object_location(S, grain, L).

state_location_objects(S, Location, Objects) :-
    pairs_value_keys(S, Location, Objects).

%?- state_start(S), state_location_objects(S, near, Objects).

state_object_location_updated(S, Object, Location, Updated) :-
    pairs_key_value_updated(S, Object, Location, Updated).

%?- state_start(S), state_object_location_updated(S, fox, left_hand, NewState).

% now we can use these to encapsulate the data representation

% Objects is the list of objects in state S.
state_objects(S, Objects) :-
    findall(Object, state_object_location(S, Object, _), Objects).

%?- state_start(S), state_objects(S, Objects).

% Holds if Object is an object in state S.
state_object(S, Object) :-
    state_objects(S, Objects),
    member(Object, Objects).

state_items(S, Items) :-
    state_objects(S, Objects),
    delete(Objects, boat, Items).

% ?- state_start(S), state_items(S, Items).

state_item(S, Item) :-
    state_items(S, Items),
    member(Item, Items).

% ?- state_start(S), state_item(S, Item).

%%%%% % web page includes a pulldown of objects, pulldown of locations, and a "Move" button that does nothing.

%?- state_start(S), state_object(S, Object).

%?- state_start(S), state_location(S, Location).

% And now we can define a state change in our world - moving an object.
% We'll represent a move with the term `object_to(Object, Location)`.
% After is state Before with Object moved to Location:
state_move_applied(Before, object_to(Object, Location), After) :-
    state_object(Before, Object),
    location(Location),
    state_object_location_updated(Before, Object, Location, After).

%?- state_start(S), state_move_applied(S, object_to(fox, farmer), NewState).

%%%% clicking Move applies the move, rerendering the page and displaying the sequence of states so far.

% Our world needs some rules, constraints, to keep us from doing impossible and dangerous things.

% we can cross the river, or move an item.

state_move(S, Move) :-
    state_move_potential(S, Move),
    Move = object_to(Object, To),
    state_object_location_updated(S, Object, To, Updated),
    state_ok(Updated).

state_move_potential(S, object_to(boat, OtherShore)) :-
    state_object_location(S, boat, Shore),
    shore(OtherShore),
    dif(Shore, OtherShore),
    state_location_objects(S, farmer, []).

state_move_potential(S, object_to(Item, To)) :-

    state_item(S, Item),

    state_object_location(S, boat, Shore),

    shore_from_to_for_item(Shore, From, To),

    % Item must come from one of the connected locations
    state_object_location(S, Item, From).

shore(S) :- member(S, [near, far]).

shore_from_to_for_item(Shore, From, To) :-
    member(From-To, [Shore-farmer, farmer-boat, boat-farmer, farmer-Shore]).

% ?- findall(To, shore_from_to_for_item(near, farmer, To), Tos).

% ?- findall(To, shore_from_to_for_item(near, near, To), Tos).

% ?- findall(To, shore_from_to_for_item(near, boat, To), Tos).

% ?- state_start(S), state_move_potential(S, object_to(fox, farmer)).

% the final state isn't actually valid, but that's the end of the puzzle.
state_ok(S) :-
    state_final(F),
    state_state_equal(S, F).

state_ok(S) :-
    state_location_object_pairs(S, ObjectsByLocation),
    maplist(location_objects_pair_ok, ObjectsByLocation).

state_location_object_pairs(S, ObjectsByLocation) :-
    findall(L-Objects,
            (location(L), state_location_objects(S, L, Objects)),
            ObjectsByLocation).

state_state_equal(S1, S2) :-
    keysort(S1, Sorted1),
    keysort(S2, Sorted2),
    Sorted1 = Sorted2.

location_objects_pair_ok(Location-Objects) :-
    location_objects_fit(Location, Objects),
    location_objects_safe_together(Location, Objects).

% the To location must be able to hold the item along with
% the items already there

location_objects_fit(Location, Objects) :-
    location_capacity(Location, C),
    length(Objects, L),
    C #>= L.

location_capacity(boat, 1).
location_capacity(farmer, 2).
location_capacity(Shore, 4) :-
    shore(Shore).

% ?- location_objects_fit(boat, []).

% ?- location_objects_fit(boat, [_]).

% ?- location_objects_fit(boat, [_, _]).

% and, of course, we can't let anyone eat anything.

% farmer is really two locations modeled as one.
location_objects_safe_together(farmer, _).

location_objects_safe_together(Location, Objects) :-
    Location \= farmer,
    list_list_cartesian_pairs(Objects, Objects, Pairs),
    maplist(item_pair_safe, Pairs).

list_list_cartesian_pairs(Xs, Ys, CartesianPairs) :-
    findall(X-Y,
            (member(X, Xs), member(Y, Ys)),
            CartesianPairs).

% ?- L = [a,b,c], list_list_cartesian_pairs(L, L, Pairs).

item_pair_safe(X-Y) :-
    \+ predator_prey(X, Y),
    \+ predator_prey(Y, X).

%?- item_pair_safe(fox-chicken).

%?- item_pair_safe(fox-grain).

% negation as failure can be tricky.
% it doesn't know anything about the fox-grain pair.

%?- item_pair_safe(fox-X).

% ?- objects_safe_together([fox, chicken]).

% ?- objects_safe_together([fox, chicken]).

% ?- objects_safe_together([fox, grain]).

% ?- objects_safe_together([grain, fox]).

% ?- location_objects_pair_ok(farmer-[fox, chicken]).

% ?- location_objects_pair_ok(near-[fox, chicken]).

% ?- location_objects_pair_ok(boat-[fox, grain]).

% ?- state_start(S), state_ok(S).

%%% web page displays validation message

% ?- state_start(S), state_move(S, object_to(fox, farmer)).

% ?- state_start(S), state_move(S, object_to(grain, farmer)).

% ?- state_start(S), state_move(S, object_to(boat, far)).

% ?- state_start(S), state_move(S, object_to(chicken, farmer)).

%%% user is trying things that can't work. can we limit the options in the ui?

state_moves_valid(S, Moves) :-
    findall(Move, state_move(S, Move), Moves).

% ?- state_start(S), state_moves_valid(S, Moves).

% ?- state_start(S), state_move_applied(S, object_to(chicken, farmer), S1), state_moves_valid(S1, Moves1).

%%% web page displays single pulldown of valid moves

% the solution is a sequence of moves that transform the
% initial state into the goal state.
solution(Transitions) :-
  state_start(Start),
  state_final(Finish),
  once(sequence_start_finish(Transitions, Start, Finish)).

sequence_start_finish(Transitions, Start, Finish) :-
    sequence_start_finish_acc(Reversed, Start, Finish, []),
    reverse(Reversed, Transitions).

% if start and finish are the same state, the sequence of states is just that.
sequence_start_finish_acc(Previous, State, Finish, Previous) :-
    state_state_equal(State, Finish).

sequence_start_finish_acc(Transitions, Start, Finish, Previous) :-
  state_moves_valid(Start, AvailableMoves),
%  print_state(Start), nl,
  include(start_previous_move_unique(Start, Previous), AvailableMoves, UniqueMoves),
  member(Move, UniqueMoves),
%  print(Move), nl,
  state_move_applied(Start, Move, NextState),
  sequence_start_finish_acc(Transitions, NextState, Finish, [Move-NextState|Previous]).

start_previous_move_unique(Start, Previous, Move) :-
    state_move_applied(Start, Move, NextState),
    maplist(value_of_pair, Previous, PreviousStates),
    state_previous_no_repeat(NextState, PreviousStates).

state_previous_no_repeat(State, Previous) :-
    include(state_state_equal(State), Previous, []).

print_state(S) :-
    keysort(S, Sorted),
    print(Sorted).

print_nl(X) :-
    print(X),
    nl.


% ?- solution(S), maplist(print_nl, S).
