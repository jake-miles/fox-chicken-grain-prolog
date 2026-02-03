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

%% There are three items we can pick up and put down.
%% Holds if the provided list is the list of items
%% the farmer must get across the river.
items([fox, chicken, grain]).
%% This is a prolog predicate, called a fact.
%% We say a predicate "holds" if its arguments maintain
%% a specific logical relationship among them.
%% `fox`, `chicken` and `grain` are the simplest kind of prolog "term"
%% (data structure) called an "atom", and [] is syntactic sugar for a list.

%% There are two shores of the river.
shores([near, far]).

shore(Shore) :-
    shores(Shores),
    member(Shore, Shores).

%% Will be useful later when we need to cross the river.
%% Holds if Shore and Other are opposite shores.
other_shore(Shore, Other) :-
    shore(Shore),
    shore(Other),
    Shore \== Other.
%% This is the other type of predicate: a rule.
%% Upper-case words are variables. A variable matches whatever it's unified against.
%% The comma means boolean-and.
%% \== means does not equal.
%% So this says that `other_shore(Shore, Other)` holds
%% if Shore is a `shore`, Other is a `shore`,
%% and they're not the same shore.

%% And there's a boat.
%% Along with the boat, there are four physical objects that can change their
%% location. The farmer remains in the boat the whole time, so no need
%% to include him as a separate object.
objects([boat|Items]) :-
    items(Items).

%% Holds if O is an object in the puzzle that can move around.
object(O) :-
    objects(Os),
    member(O, Os).

%% The boat is both an object that can move and a location
%% where the farmer can set down an item.
locations([boat|Shores]) :-
    shores(Shores).

%% Holds if L is a location where the farmer can set down an item.
location(L) :-
    locations(Ls),
    member(L, Ls).

%% Foxes eat chickens and chickens eat grain.
predator_prey(fox, chicken).
predator_prey(chicken, grain).
%% A predicate can have multiple "branches". When the predicate is invoked,
%% prolog will first match with the firt branch, and then on backtracking
%% will match the others. In this way they define a boolean-or.
%% For clarity, since we're defining a relationship among arguments,
%% the preferred naming convention for a predicate
%% is the names of the arguments in order, using argument names
%% that make the relationship clear. So instead of a name like `eats`, we use
%% `predator_prey`. Clear predicate names can require a lot of careful thought.

%% Since they will eat each other, the farmer must always be present with them.
%% Holds if X and Y mut be supervised when together.
must_be_supervised(X, Y) :-
    predator_prey(X, Y).
%% This is a prolog rule - a predicate that calls others.

must_be_supervised(X, Y) :-
    predator_prey(Y, X).
%% And the rule has two branches.
%% So X and Y need to be supervised if X eats Y (the branch above)
%% or if Y eats X (this branch).

%% Conversely, they can be left alone together if they don't need supervision.
%% Holds if X and Y can be left alone together.
can_be_left_alone(X, Y) :-
    \+ must_be_supervised(X, Y).
%% \+ means that the clause succeeds if the goal to its right fails, i.e.
%% can't be proven true. This is called "negation as failure".
%% It's similar, but not identical, to boolean-not.
%% So this means that X and Y can be left alone if X and Y don't need
%% to be supervised together.

%% Holds if Item can be left alone with all the items in Items.
items_safe_with_item(Items, Item) :-
    maplist(can_be_left_alone(Item), Items).
%% Prolog supports higher-order predicates and partial application.
%% This creates an anonymous, partially-applied predicate by applying
%% `Item` as the first argument to `can_be_left_alone`, and then
%% maps that anonymous predicate over the list `Items`.
%% This succeeds if `can_be_left_alone(Item)`
%% succeeds for all items in `Items`.

%% Holds if Items are all safe if left alone together
items_safe(Items) :-
    maplist(items_safe_with_item(Items), Items).
%% here we're doing a double-loop over Items, confirming all
%% the items are pairwise safe with each other.

%% The farmer can hold up to two items.
%% Holds if the farmer can hold the given list of items.
farmer_can_hold_items([]).
farmer_can_hold_items([_]).
farmer_can_hold_items([_,_]).
%% Each branch here unifies with a different structure, or pattern, of
%% a list of items a farmer can hold - zero, one or two items. The
%% underscore `_` matches any value and ignores it.

%% The boat can only hold at most one item.
%% Holds if the boat can hold the given list of items.
boat_can_hold_items([]).
boat_can_hold_items([_]).

%% The above definitions are all static - they don't need any dynamically
%% changing information about the world to evaluate themselves.
%% But the rules depend on conditions that can change
%% over time, so we introduce an argument to the next predicates representing the
%% current state of the world. The predicates for that data oblect's
%% accessors, like state_item_location and state_location_entities,
%% are at the end of the file.

%% The state data representation.
%% We'll represent the state as a list of Key-Value pairs,
%% where the key is one of the elements in our story, and
%% the value is that element's location. The predicates
%% This accessor is an encapsulation predicate for that list of pairs.
%% Holds if in state S, Entity is at Location.
state_entity_location(S, Entity, Location) :-

    %% a data integrity check:
    %% the valid values for Entity
    object(Objects).
    member(Entity, Objects),

    %% and for Location
    locations(Locations),
    member(Location, Locations),

    member(Entity-Location, S).
%% In prolog, a pair is conventionally represented as Key-Value with a hyphen.
%% The hyphen isn't special syntax. It's an operator defined in the ISO
%% standard, but we're free to define our own syntactic operators, giving
%% them any meaning we like. Here we use `member` to unify with the
%% element in S with the key Entity and the value Location.

%% and accessors into the state object

%% Objects is a list of object at Location in state S.
state_location_entities(S, Location, Objects) :-
    findall(Entity, state_entity_location(S, Entity, Location), Objects).

%% The boat is at Location in state S.
state_boat_location(S, Location) :-
    state_entity_location(S, boat, Location).

%% Item is at Location in state S.
state_item_location(S, Item, Loc) :-
    state_entity_location(S, Item, Loc).

state_item_is_on_boat(S, Item) :-
    state_item_location(S, Item, boat).

%% The farmer is holding Item in state S.
state_item_held(S, Item) :-
    state_entity_location(S, Item, held).

%% The farmer is holding Items in state S.
state_items_held(S, Items) :-
    state_location_entities(S, held, Items).

%% Items is a list of items at Location
state_location_items(S, Location, Items) :-
    state_location_entities(S, Location, Objects),
    include(item, Objects, Items).

%% Now that we can represent the world state,
%% back to the problem domain! The state-dependent rules.

state_farmer_can_hold_items(S) :-
    state_items_held(S, Items),
    farmer_can_hold_items(Items).

%% The boat can only hold one item at a time.
state_boat_can_hold_items(S) :-
    state_location_entities(S, boat, Items),
    boat_can_hold_items(Items).

%% Whether predators can eat their prey is also something
%% that changes in our world, so we capture that with a state-dependent rule
%% determining whether prey can be eaten by a predator in that state.
state_safe(S) :-
    shores(Shores),
    maplist(state_location_safe(S), Shores).

state_location_safe(S, Location) :-
    state_location_items(S, Items),
    items_safe(Items).

%% all of a valid state's rules in one list.
state_rules([state_farmer_can_hold_items,
             state_boat_can_hold_items,
             state_safe]).

%% The initial state of the puzzle: everything is on the near shore.
state_initial(S) :-
    object(Objects),
    state_location_entities(S, near, Objects).

%% The goal state: everything is on the far shore.
state_goal(S) :-
    object(Objects),
    state_location_entities(S, far, Objects).

%% The solution to the puzzle is a list of moves that transform the initial
%% state stepwise into the goal state.
solution(Moves) :-
    state_initial(Start),
    state_goal(Goal),
    %% defined a little farther down
    state_moves_applied(Start, Moves, Goal).
%% todo: don't revisit states

%% The actions available to the farmer to find a solution.
%% We can pick an item up, put an item down, and cross the river.

move(farmer_picks_up(Item)) :-
    item(Item).
%% `farmer_picks_up` is a complex "term", or data structure, defined inline.
%% `move` will only succeed if you call it with a term that unifies with
%% (matches the structure of) this one or that of the other branches of
%% `move`.

move(farmer_puts_down(Item)) :-
    item(Item).

move(cross_river).

%% Rules about how the physical world works.
%% i.e. in what states are moves physically possible.

state_move_possible(S, farmer_picks_up(Item)) :-
    \+ state_item_held(S, Item),
    state_item_at_boat(S, Item).

state_move_possible(S, farmer_puts_down(Item)) :-
    state_item_held(S, Item),
    state_item_at_boat(S, Item).

state_move_possible(S, cross_river) :-
    %% farmer needs his hands free to row the boat.
    state_items_held([]).

%% some helper predicates

%% Holds if Item is on or next to the boat.
state_item_at_boat(S, Item) :-
    state_item_is_on_boat(S, Item).

state_item_at_boat(S, Item) :-
    state_boat_location(S, Location),
    state_item_location(S, Location, Item).


%% Define our very simple algorithm of deriving the list of moves.
%% Case one:
%% If we're already at the goal state,
%% the list of moves to get there is the empty list.
state_moves_applied(Goal, [], Goal).

%% Case two:
%% the list is a first move `First` that gets us to some new state `S`,
%% followed by a list of moves `Rest` that gets us from `S` to `Goal`.
state_moves_applied(Start, [First|Rest], Goal) :-
    state_move_possible(Before, Move),
    state_move_applied(Before, Move, After),
    state_valid(After),
    state_moves_applied(Rest, S, Goal).

%% Relates a state `Before` to a state `After`,
%% the result of applying the move `farmer_picks_up` to `Before`.
state_move_applied(Before, farmer_picks_up(Item, Location), After) :-

    state_items_held(Before, ItemsHeldBefore),

    %% the resulting `After` state
    state_items_held(After, [Item|ItemsHeldBefore]).

state_move_valid(S, farmer_puts_down(Item, Location)) :-
    state_item_held(S, Item),
    state_boat_location(S, Location),
    state_location_entities(S, ItemsAtLocation),
    item_safe_with_items(Item, ItemsAtLocation).

state_move_applied(Before, farmer_puts_down(Item, Location), After) :-
    state_held_items(Before, ItemsHeldBefore),
    select(Item, ItemsHeldBefore, ItemsHeldAfter),
    state_items_held(After, ItemsHeldAfter),
    state_item_location(After, Item, Location).

state_move_applied(Before, cross_river, After) :-
    state_boat_location(Before, ShoreBefore),
    other_shore(ShoreBefore, ShoreAfter),
    state_boat_location(After, ShoreAfter).

%% A little rules "engine".
%% Given a state of the world `S`,
%% FailedRules is the list of rule predicates that fail
%% for the given state of the world `S`.
state_rules_applied(S, FailedRules) :-
    state_rules(Rules),
    maplist(state_rule_applied(S), Results),
    exclude(\X^(X \== pass), Results, FailedRules).
%% prolog supports lambdas. The syntax is a bit odd.

state_rule_applied(State, Rule, Result) :-
    %% scryer prolog's declarative `if` predicate
    if_(call(Rule, State),
        pass,
        Rule).

%% Succeeds if no rules fail in state S.
state_valid(S) :-
    state_rules_applied(S, []).
