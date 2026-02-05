:- use_module(library(builtins)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(clpz)).

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

%% The four things in this puzzle world that can move.
%% The farmer moves with the boat, so no need to represent his motion.
movable(boat).
movable(item(fox)).
movable(item(chicken)).
movable(item(chicken)).

/*
  Now we need to define some rules about the physical world,
  i.e. how things are allowed to move.

  Multiple ways we can represent this, but we'll definer
  each type of movable object's valid path through the locations.
*/

movable_path(Movable, Path) :-
    movable_path_(Movable, Path).

%% An object can move back and forth along its path.
movable_path(Movable, Reverse) :-
    movable_path_(Movable, Path),
    reverse(Path, Reverse).

%% An item can move from shore => farmer's hands => boat, and back.
movable_path_(item(_), [shore(_), hands, boat]).

%% The boat can move from the near shore to the far shore and back.
movable_path_(boat, [shore(near), shore(far)]).

movable_step(Movable, [From-To]) :-
    movable_path(Movable, Path),
    member([From-To], Path).

/*
  Now we represent the danger.
*/
predator_prey(fox, chicken).
predator_prey(chicken, grain).

/*
  location_items_ok(Location, Items).
  Holds if Items can all exist at Location together
  according to the rules of the puzzle.
*/

%% only one item in the boat at a time.
location_items_ok(boat, Items) :-
    list_length_lte(Items, 1).

%% at most 2 items in the farmer's hands at a time.
location_items_ok(hands, Items) :-
    list_length_lte(Items, 2).

%% Items can be placed together on a shore if they're all safe when left alone.
location_items_ok(Location, Items) :-
    shore(Location),
    safe_alone(Items).

%% Holds if items can be safely left alone together.
safe_alone(Items) :-
    maplist(safe_alone_(Items), Items).

safe_alone_(Items, item(Item)) :-
    maplist(\OtherItem^(\+ predator_prey(Item, OtherItem))).

%% Holds if list L has a length <= Max.
list_length_lte(L, Max) :-
    length(L, Length),
    Length #=< Max.

/*
  We need a way to hold an object and a location together, to
  represent that the object is at that location. Rather than
  use a verbose term like `object_location(Object, Location)`, we can
  use a concise custom operator, @. Prolog conventionally uses a hyphen, `-`,
  to denote a pair, but in this case the @ conveys the meaning more precisely.

  This is a custom operator directive. It has no meaning to prolog,
  It's used in pattern matching as part of the term's structure like
  in non-operator pattern matching. This just tells prolog that we're
  going to use the @ symbol in some of our terms so it doesn't think
  it's a syntax error, and assigns it a precedence so prolog knows how
  to group things when parsing a term containing multiple operators.

  Object@Location denotes Object's placement at Location.
*/
op("@", xfx, 500).

/*
  To express the rest of the problem and its solution, which involves
  moving objects around, i.e. changing the state of the world, our predicates
  need to know the state of the world they're matched against,
  and we need a data representation for the state of the world.
  We'll represent it as a list of Object@Location, hiding that
  representation using the following accessors.
*/

%%%%%%%%%%%%%%% state accessors

%% state_placement(S, P).
%% Holds if P is a placement in state S.
state_placement(S, Placement)) :-
    member(Placement, S).

/*
  state_placement_applied(Before, Placement, After).
  Holds if, given an initial state Before,
  applying Placement produces state After.
*/
state_placement_applied(Before, Placement, [Placement|Others]) :-
    select(Placement, Before, Others).

%% Holds if in state S, Objects are all placed at Location.
state_location_objects(S, Location, Objects) :-
    findall(Object, state_placement(S, Object@Location), Objects).

%%%%%%%%%%%%%%%%

/* The predicates below all depend on the state of the world,
   so they'll all take the state as an argument, `S`. In the
   case of a predicate that represents a state change,
   the predicate will take two state arguments, `Before` and `After`.
*/

%% Holds if S is the initial puzzle state.
state_initial(S) :-
    state_all_at_location(S, shore(near)).

%% Holds if S is the gaol puzzle state.
state_goal(S) :-
    state_all_at_location(S, shore(far)).

%% Holds if in state S, all movables are at Location.
state_all_at_location(S, Location) :-
    findall(M, movable(M), Movables),
    state_location_objects(S, Shore, Movables).

%% Holds if from state S, Object is physically able to move to location To.
state_placement_possible(S, Object@To) :-

    %% the item's location in state S
    state_placement(S, Object@From),

    %% there's a valid step From => To defined for Object's path.
    movable_step(Object, [From, To]).

/*
  Holds if in state S, Object can be placed in location To
  according to the rules of the puzzle.
*/
state_placement_allowed(S, Object@To) :-

    %% The items in location To
    state_location_items(S, To, AlreadyThere),

    %% We can add Item to that list according to the rules of the puzzle.
    location_items_ok(To, [Object|AlreadyThere]).

%% And now the algorithm to find solutions.

/*
  Holds if Moves is a sequence of legal moves that transform
  the initial state stepwise into the goal state.
*/
solution(Moves) :-
    state_initial(Initial),
    state_goal(Goal),
    moves_from_to(Moves, Initial, Goal).

/*
  moves_from_to(Moves, Initial, Goal).
  Holds if Moves is a sequence of legal moves that
  transform the Initial state into the Goal state.
*/
moves_from_to([], Goal, Goal),
moves_from_to([First|Rest], Initial, Goal) :-
    move_before_after(First, Initial, S),
    moves_from_to(Rest, S, Goal).

%% Holds if Move is a valid placement from state Before to state After.
move_before_after(Move, Before, After) :-
    state_placement_possible(Before, Move),
    state_placement_allowed(After, Move),
    state_placement_applied(Before, Move, After).
