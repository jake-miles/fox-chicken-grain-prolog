:- use_module(library(builtins)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(clpz)).
:- use_module(library(pairs)).

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

/*
  Prolog to the rescue.

  The solution will be a sequence of moves, each of an item to a new
  location.
*/

/*
  For conciseness, we'll define a couple custom operators.
  The custom operator has no meaning to prolog; It just becomes part
  of a term's pattern as it's matched against other terms. The `op`
  directive just tells prolog that we're going to use the symbol we're
  defining in some of our terms so it doesn't think it's a syntax
  error, and we assign it a precedence so prolog knows how to group
  things when parsing a term containing multiple operators. The `xfx`
  means it's an infix operator.
*/

item(fox).
item(chicken).
item(grain).

%% The four things in this puzzle world that can move.
%% The farmer moves with the boat, so no need to represent his motion.
movable(boat).
movable(X) :-
    item(X).

/*
  ?- movable(X).
  %@    X = boat
  %@ ;  X = fox
  %@ ;  X = chicken
  %@ ;  X = grain.

  ?- findall(X, movable(X), Xs).
  %@    Xs = [boat,fox,chicken,grain].
*/


/*
  We need to model the physical world of the puzzle,
  This includes which locations are one step away from
  which others, and how many items each location can hold at once.
*/

movable_step_(boat, [shore(near), shore(far)]).
movable_step_(item(_), [shore(_), hands]).
movable_step_(item(_), [hands, boat]).

movable_step(Movable, [From, To]) :-
    %% semicolon means boolean-or. You put it at the beginning of
    %% the line by convention so it stands out from the comma,
    %% which means boolean-and.
    (
        movable_step_(Movable, [From, To])
    ;   movable_step_(Movable, [To, From])
    ).

/*
  ?- movable_step(boat, [X, Y]).
  %@    X = shore(near), Y = shore(far)
  %@ ;  X = shore(far), Y = shore(near).

  ?- movable_step(item(chicken), [X, Y]).
  %@    X = shore(_A), Y = hands
  %@ ;  X = hands, Y = boat
  %@ ;  X = hands, Y = shore(_A)
  %@ ;  X = boat, Y = hands.

  ?- movable_step(item(chicken), [shore(near), Y]).
  %@    Y = hands
  %@ ;  false.
*/

%% location_limit(Location, Limit)
%% Holds if Location can hold at most Limit items.
location_limit(boat, 1).
location_limit(hands, 2).
location_limit(shore(_), 3).

%% Holds if Location can physically hold all the items in Items.
location_items_fit(Location, Items) :-
    %% comma means boolean-and
    length(Items, Count),
    location_limit(Location, Limit),
    Count #=< Limit.

/*
  ?- location_items_fit(boat, [item(fox), item(chicken), item(grain)]).
  %@    false.
  ?- location_items_fit(boat, [item(fox)]).
  %@    true.
  ?- location_items_fit(shore(near), [item(fox), item(chicken), item(grain)]).
  %@    true.
*/

/*
  And the last component of the puzzle to model:
  some of our items want to eat each other.
*/

%% predator_prey(Predator, Prey).
%% Holds if Predator will eat Prey if left alone with it.
predator_prey(fox, chicken).
predator_prey(chicken, grain).

location_items_safe(hands, _).
location_items_safe(boat, _).

%% Items can be placed together on either shore
%% as long as none of them want to eat each other.
location_items_safe(shore(_), Items) :-
    cartesian_product(Items, Items, Pairs),
    %% prolog supports higher-order predicates like maplist.
    maplist(not_predator_prey, Items).

not_predator_prey(X-Y) :- \+ predator_prey(X, Y).

%% Holds if Items can coexist together at Location.
location_items_ok(Location, Items) :-
    location_items_fit(Location, Items),
    location_items_safe(Location, Items).

/*
  ?- location_items_ok(boat, [fox, chicken]).
  %@    false.
  ?- location_items_ok(boat, [fox]).
  %@    true.
  ?- location_items_ok(hands, [fox, chicken]).
  %@    true.
  ?- location_items_ok(hands, [fox, chicken, grain]).
  %@    false.
  ?- location_items_ok(shore(_), [fox, chicken]).

*/


/*
  The predicates below all depend on the changing state of the world,
  so they'll all take the state of the world as an argument, `S`. In
  the case of a predicate that represents a state change, the
  predicate will take two state arguments, `Before` and `After`.

  The state representation is defined at the end of the file.
  It's defined in terms of three accessors:

  %% state_placement(S, Placement).
  %% Holds if Placement is an Object@Location in state S.

  %% state_placement_applied(Before, Placement, After).
  %% Holds if, given an initial state Before,
  %% applying Placement produces state After.

  %% state_location_objects(S, Location, Objects)
  %% Holds if in state S, Objects are all @Location.
*/

/*
  This captures the logic of a valid next move.

  Holds if from state S, Object can be placed in location `To`
  according to the rules of the puzzle.
*/
state_placement_ok(S, at(Object, To)) :-

    %% Object is @From in state S.
    state_placement(S, at(Object, From)),

    %% [From, To] is one of Object's defined movable_steps.
    movable_step(Object, [From, To]),

    %% The items already in location To
    state_location_items(S, To, AlreadyThere),

    %% it's ok to add Object to that list at location To.
    location_items_ok(To, [Object|AlreadyThere]).

%% S is the initial puzzle state.
state_initial(S) :-
    state_all_at_location(S, shore(near)).

%% S is the goal puzzle state.
state_goal(S) :-
    state_all_at_location(S, shore(far)).

%% Holds if in state S, all movables are at Location.
state_all_at_location(S, Location) :-
    findall(M, movable(M), Movables),
    state_location_objects(S, Location, Movables).

/*
  Holds if Moves is a sequence of legal moves that transform
  the initial state stepwise into the goal state.
*/
solution(Moves) :-
    state_initial(Initial),
    state_goal(Goal),
    moves_from_to(Moves, Initial, Goal).

%% And now the simple algorithm to find solutions.

/*
  moves_from_to(Moves, Initial, Goal).
  Holds if Moves is a sequence of legal moves that
  transform the Initial state into the Goal state.
*/
moves_from_to([], Goal, Goal),
moves_from_to([First|Rest], Initial, Goal) :-
    move_before_after(First, Initial, S),
    moves_from_to(Rest, S, Goal).

%% Holds if Move is a valid placement moving from state Before to
%% state After.
move_before_after(Move, Before, After) :-
    state_placement_ok(After, Move),
    state_placement_applied(Before, Move, After).


%%%%%%%%%%%%%%% state accessors
%% State is a list of placements, i.e. of Object@Location terms.

%% state_placement(S, Placement).
%% Holds if Placement is an Object@Location in state S.
state_placement(S, Placement) :-
    member(Placement, S).

/*
  state_placement_applied(Before, Placement, After).
  Holds if, given an initial state Before,
  applying Placement produces state After.
*/
state_placement_applied(Before, Placement, [Placement|Others]) :-
    %% Others is Before without Placement
    select(Placement, Before, Others).

%% Holds if in state S, Objects are all placed at Location.
state_location_objects(S, Location, Objects) :-
    findall(Object, state_placement(S, at(Object, Location)), Objects).

%%%%%%%%%%%%%%%%

%% Holds if Product is a list of all pairings of the elements of Xs and Ys.
cartesian_product(Xs, Ys, Product) :-
    findall(X-Y,
            (
             member(X, Xs),
             member(Y, Ys)
            ),
            Product).
