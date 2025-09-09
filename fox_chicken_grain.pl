:- use_module(library(builtins)).
:- use_module(library(lists)).

farmer.

item(fox).
item(chicken).
item(grain).

location(boat).
location(shore(near)).
location(shore(far)).

animal_eats(fox, chicken).
animal_eats(chicken, grain).

item_not_allowed_with(X, Y) :-
    animal_eats(X, Y),
    animal_eats(Y, X).

boat_can_hold_items([]).
boat_can_hold_items([_]).

farmer_can_hold_items(Items)  :-
    length(Items, N),
    N #< 3.

initial_state(S) :-
    state_location_entities(S, shore(near), [fox, chicken, grain, farmer]).

goal_state(S) :-
    state_location_entities(S, shore(far), [fox, chicken, grain, farmer]).

% solution is a sequence of actions that gets all items
% and the farmer safely across the river.
solution(Actions) :-
    initial_state(S0),
    goal_state(S),
    safe_actions_from_to(Actions, S0, S).

%%% the search stratagy - the algorithm

safe_actions_from_to([], State, State), !.

safe_actions_from_to([Action|Actions], State0, State) :-
    safe_action_before_after(Action, State0, State1),
    safe_actions_from_to(Actions, State1, State).

%%%

safe_action_before_after(Action, Before, After) :-
    action(Action),
    state_action_possible(Action, Before),
    action_before_after(Action, Before, After),
    safe_state(After).

safe_state(S) :-
    \+ unsafe_state(S).

unsafe_state(S) :-
    location(L),
    unsafe_state_location(S, L).

unsafe_state_location(State, Location) :-
    item(Item),
    state_location_entity(State, Location, Item),

    item(OtherItem),
    state_location_entity(State, Location, OtherItem),

    item_not_allowed_with(Item, OtherItem).


%%% static action definitions: what can happen in this world.

action(move_from_to(farmer, shore(_), boat)).

action(move_from_to(farmer, boat, shore(_))).

action(move_from_to(boat, shore(Shore1), shore(Shore2))) :-
    Shore1 \= Shore2.

action(farmer_picks_up(Item)) :-
    item(Item).

action(farmer_puts_down(Item)) :-
    item(Item).

%%% dynamic action rules:
%%% when each action is possible given the current state of the world.

state_action_possible(State, move_from_to(farmer, shore(Shore), boat)) :-
    state_location_entity(State, Shore, farmer),
    state_location_entity(State, Shore, boat).

state_action_possible(State, move_from_to(farmer, boat, shore(Shore))) :-
    state_location_entity(State, boat, farmer),
    state_location_entity(State, Shore, boat).

state_action_possible(State, move_from_to(boat, shore(Shore), shore(_))) :-
    state_location_entity(State, Shore, boat),
    state_farmer_can_row_boat(State).

state_action_possible(State, farmer_picks_up(Item)) :-
    \+ state_item_held(State, Item),
    state_location_entity(State, Location, Item),
    state_location_entity(State, Location, farmer),
    state_item_on_ground(State, Item),
    state_items_held(State, Held),
    farmer_can_hold_items([Item|Held]).

state_action_possible(State, farmer_puts_down(Item)) :-
    state_item_held(State, Item),
    state_location_entiity(State, Location, Item),
    state_location_can_hold_item(State, Location).

state_location_can_hold_item(State, shore(_)).

state_location_can_hold_item(State, boat) :-
    state_location_entities(State, boat, []).

state_location_entities(State, boat, [farmer]).

%%% How each action relates a state to the state following the action

action_before_after(move_from_to(farmer, shore(Shore), boat), Before, After) :-
    entity_move(farmer, Before-shore(Shore), After-boat).

action_before_after(move_from_to(farmer, boat, shore(Shore)), Before, After) :-
    entity_move(farmer, Before-boat, After-shore(Shore)).

action_before_after(move_from_to(boat, shore(Shore1), shore(Shore2)), Before, After) :-
    entity_move(boat, Before-shore(Shore1), After-shore(Shore2)).

action_before_after(farmer_picks_up(Item), Before, After) :-
    key_before_after(state_items_held, Before-WithoutItem, After-WithItem),
    select(Item, WithItem, WithoutItem).

action_before_after(farmer_puts_down(Item), Before, After) :-
    action_before_after(farmer_picks_up(Item), After, Before).


%%% accessors

state_location_entity(State, Location, Entity) :-
    state_key_value(State, location-Entity, Location).

state_location_entities(State, Location, Entities) :-
    maplist(call(state_location_entity(State, Location)), Entities).

state_items_held(State, Items) :-
    state_key_value(State, items_held, Items).

state_item_held(State, Item) :-
    state_items_held(State, Items),
    member(Item, Items).

state_item_on_ground(State, Item) :-
    \+ state_item_held(State, Item).

entity_move(Entity, State0-Location0, State-Location) :-
    key_before_after(location-Entity, State0-Location0, State-Location).

state_key_value(State, Key, Value) :-
    pairs_key_value(State, Key, Value).


%%% data representation

key_before_after(Key, S0-V0, S-V) :-
    pairs_without_key(S, Key, S1),
    pairs_with_pair(S1, Key-V, S).

pairs_key_value([Key-Value|_], Key, Value), !.

pairs_key_value([_|Pairs], Key, Value) :-
    pairs_key_value(Pairs, Key, Value).

pairs_without_key([], _, []).

pairs_without_key([Key-_|Pairs], Key, PairsWithout) :-
    pairs_without_key(Pairs, Key, PairsWithout).

pairs_without_key([Key0-V_|Pairs], Key, PairsWithout) :-
    Key0 \= Key,
    pairs_without_key(Pairs, Key, [Key0-V|PairsWithout]).

pairs_with_pair(Pairs0, Key-Value, [Key-Value|Pairs]).
