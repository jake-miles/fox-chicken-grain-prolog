:- use_module(library(builtins)).
:- use_module(library(lists)).

%% The fox, chicken and grain puzzle, in prolog using a
%% depth-first search of the problem space.
%%
%% There's a farmer in a boat on the shore of a river.
%% On the riverbank he has three items: a fox, a chicken and a bag of grain.
%% He needs to get them all across the river.
%%
%% The fox wants to eat the chicken, so they must always be kept apart.
%% The chicken wants to eat the grain, so they must always be kept apart.
%%
%% The farmer is able to hold up to
%% two of the three items at a time, so wherever he is,
%% he can keep the fox from eating the chicken and the chicken from eating
%% the grain, but he can't leave them alone together.
%%
%% But he needs both hands to row the boat, and he can only fit
%% one item in the boat with him at a time.
%%
%% What sequence of moves will let the farmer get the fox, chicken and grain across
%% the river?

%% In prolog:

%% There are three items.
item(fox).
item(chicken).
item(grain).
%% This is a prolog predicate with three disjunctive (boolean-or) "branches".
%% `fox`, `chicken` and `grain` are the simplest kind of prolog "term"
%% (data structure) called an "atom".

%% There are two shores of the river we can toggle between.
other_shore(near, far).
other_shore(far, near).
%% `near` and `far` are also atoms.

%% With the boat, there are three locations total where we can set the items down.
location(near).
location(far).
location(boat).
%% These `near` and `far` atoms are the same as the `near` and `far`
%% atoms referred to in `other_shore` above. i.e. an atom always
%% matches itself.

%% Foxes eat chickens and chickens eat grain.
predator_prey(fox, chicken).
predator_prey(chicken, grain).
%% For clarity, since we're defining a relationship among arguments,
%% the preferred naming convention for a predicate
%% is the names of the arguments in order, using argument names
%% that make the relationship clear. So instead of a name like `eats`, we use
%% `predator_prey`. Clear predicate names can require a lot of careful thought.

%% Since they will eat each other, the farmer must always be present with them.
must_be_supervised(X, Y) :-
    predator_prey(X, Y).
%% This is a prolog rule - a predicate that calls others.

must_be_supervised(X, Y) :-
    predator_prey(Y, X).
%% And the rule has two branches.
%% So X and Y need to be supervised if X eats Y (the branch above)
%% or if Y eats X (this branch).

%% Conversely, they can be left alone together if they don't need supervision.
can_be_left_alone(X, Y) :-
    \+ must_be_supervised(X, Y).
%% \+ means that the clause succeeds if the goal to its right fails, i.e.
%% can't be proven true. This is called "negation as failure".
%% It's similar, but not identical, to boolean-not.
%% So this means that X and Y can be left alone if X and Y don't need
%% to be supervised together.

%% Succeeds if Item can be left alone with all the items in Items.
item_safe_with_items(Item, Items) :-
    maplist(can_be_left_alone(Item), Items).
%% Prolog supports higher-order predicates and partial application.
%% This creates an anonymous, partially-applied predicate by applying
%% `Item` as the first argument to `can_be_left_alone`, and then
%% maps that anonymous predicate over the list `Items`.
%% This succeeds if `can_be_left_alone(Item)`
%% succeeds for all items in `Items`.

%% The farmer can hold up to two items
farmer_can_hold_items([]).
farmer_can_hold_items([_]).
farmer_can_hold_items([_,_]).
%% Each branch here unifies with a different
%% cases of a list of items a farmer can hold - zero, one or two items.
%% The underscore `_` matches any value and ignores it.
%% This could also be implemented as a rule that checks the list's length.

%% The three moves we have available to find a solution:
%% picking an item up, putting it down, and crossing the river.

move(farmer_picks_up(Item, Location)) :-
    item(Item),
    location(Location).
%% `farmer_picks_up` is a complex term, a data structure defined inline.
%% `move` will only succeed if you call it with a term that unifies with
%% (matches the structure of) this one or that of the other branches of
%% `move`.

move(farmer_puts_down(Item, Location)) :-
    item(Item),
    location(Location).

move(cross_river).

%% But the farmer can only cross the river with his hands free,
%% and with at most one item in the boat.
state_can_cross_river(S) :-
    state_items_held(S, []),
    (
        state_items_in_boat(S, [])
    ;   state_items_in_boat(S, [_])
    ).
%% Here we introduce an argument representing the current state of the world, `S`.
%% We'll pick a data representation for `S` later, for now assuming the
%% existence of accessors like `state_items_held`, which relates a state
%% to the list of items the farmer is holding.
%% The comma means boolean-and, and the semicolon
%% means boolean-or. The semicolon is an alternative to writing two
%% branches for a predicate the way we did for the predicates above.

%% The initial state of the puzzle: everything on the near shore.
state_initial(S) :-
    state_location_items(S, near, [fox, chicken, grain]),
    state_boat_location(S, near).

%% The goal state: everything on the far shore.
state_goal(S) :-
    state_location_items(S, far, [fox, chicken, grain]),
    state_boat_location(S, far).

%% The solution to the puzzle is a list of moves that transform the initial
%% state stepwise into the goal state.
solution(Moves) :-
    state_initial(Start),
    state_goal(Goal),
    moves_start_goal(Moves, Start, Goal).

%% Define our very simple algorithm of deriving the list of moves.
%% Case one:
%% If we're already at the goal state,
%% the list of moves to get there is the empty list.
moves_start_goal([], Goal, Goal).

%% Case two: the list is a first move `First` that gets us to a new state `S`,
%% followed by a list of moves `Rest` that gets us from `S` to `Goal`.
moves_start_goal([First|Rest], Start, Goal) :-
    move_before_after(First, Start, S),
    moves_start_goal(Rest, S, Goal).

%% Relates a move `Move` to its valid `Before` and `After` states.
move_before_after(Move, Before, After) :-
    state_move_valid(Before, Move),
    state_move_applied(Before, Move, After).

%% Relates a state `S` to a valid `farmer_picks_up` move in state `S`.
state_move_valid(S, farmer_picks_up(Item, Location)) :-

    %% the farmer isn't already holding `Item`.
    \+ state_item_held(S, Item),

    %% the farmer is at the same location as `Item`.
    state_item_with_farmer(S, Item).

%% Relates a state `Before` to a state `After`,
%% the result of applying the move `farmer_picks_up` to `Before`.
state_move_applied(Before, farmer_picks_up(Item, Location), After) :-

    state_items_held(Before, ItemsHeldBefore),

    ItemsHeldAfter = [Item|ItemsHeldBefore],

    %% the farmer has at least one hand free
    farmer_can_hold_items(ItemsHeldAfter),

    %% the resulting `After` state
    state_items_held(After, ItemsHeldAfter).

state_move_valid(S, farmer_puts_down(Item, Location)) :-
    state_item_held(S, Item),
    state_farmer_location(S, Location),
    state_location_items(S, ItemsAtLocation),
    item_safe_with_items(Item, ItemsAtLocation).

state_move_applied(Before, farmer_puts_down(Item, Location), After) :-
    state_held_items(Before, ItemsHeldBefore),
    select(Item, ItemsHeldBefore, ItemsHeldAfter),
    state_held_items(After, ItemsHeldAfter),
    state_item_location(After, Item, Location).

state_move_valid(S, cross_river) :-
    state_can_cross_river(S).

state_move_applied(Before, cross_river, After) :-
    state_boat_location(Before, ShoreBefore),
    other_shore(ShoreBefore, ShoreAfter),
    state_boat_location(After, ShoreAfter).

state_item_held(S, Item) :-
    state_items_held(S, ItemsHeld),
    member(Item, ItemsHeld).

state_item_with_farmer(S, Item) :-
    state_location_farmer(S, Location),
    state_location_item(S, Location, Item).

%% todo: define accessors and state and move pretty-printing
%% todo: don't revisit states
