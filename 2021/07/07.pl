['../prolog/utils'].

median(L, Median) :-
    msort(L, S),
    median_(S, Median).

median_(L, Median) :-
    append([Left, [Median], Right], L),
    same_length(Left, Right).
median_(L, Median) :-
    append([Left, [X, Y], Right], L),
    same_length(Left, Right),
    Median is (X + Y) / 2.

crab_and_X_pos(X, Fuel, Crab, FuelCost) :-
    X #= Crab + Fuel,
    FuelCost #= abs(Fuel).

% crabs: [16,1,2,0,4,2,7,1,2,14]
fuel_cost(Crabs, X, Cost) :-
    Cost #>= 0,
    same_length(Crabs, Fuels),
    maplist(crab_and_X_pos(X), Fuels, Crabs, FuelCosts),
    sum(FuelCosts, #=, Cost),
    labeling([min(Cost)], [Cost]).

% 604539, too high, Fuels ins -1000..1000
solution_07_01(Cost) :-
    slurp(Line_, '07/input.txt'),
    nth1(1, Line_, Line),
    split_string(Line, ',', '', NumChars),
    maplist(atom_number, NumChars, Crabs),
    median(Crabs, XMedian),
    fuel_cost(Crabs, XMedian, Cost).
