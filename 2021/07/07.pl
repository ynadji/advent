/*
This one was trickier than I liked. I was originally hoping to just use the
CLP(FD) predicates and feel smug about my choice to use Prolog, but the
constraints were too wide to just work off of that. I noticed that the median
for the example for part 1 matched the brute forced value for X, so I tried
the same for the complicated one and it worked. The mean worked in the second
case. I would've preferred to have just bounded the search space around those
values, but I was unable to get that to work properly :(.
*/
['../prolog/utils'].

mean(L, Mean) :-
    sum_list(L, Sum),
    length(L, Len),
    Mean #= Sum // Len.

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

n_factorial(0, 1).
n_factorial(N, F) :-
    N #> 0,
    N1 #= N - 1,
    F #= N * F1,
    n_factorial(N1, F1).

binomial(N, K, B) :-
    n_factorial(N, NF),
    n_factorial(K, KF),
    N_K #= N - K,
    n_factorial(N_K, N_KF), !,
    B is NF / (KF * N_KF).

triangle(0, 0).
triangle(X, T) :-
    X_ #= X + 1,
    binomial(X_, 2, T).

crab_and_X_pos(X, Fuel, Crab, FuelCost) :-
    X #= Crab + Fuel,
    FuelCost #= abs(Fuel).

crab_and_X_pos_2(X, Fuel, Crab, FuelCost) :-
    X #= Crab + Fuel,
    AbsFuel #= abs(Fuel),
    triangle(AbsFuel, FuelCost).

% crabs: [16,1,2,0,4,2,7,1,2,14]
fuel_cost(Crabs, X, Cost) :-
    Cost #>= 0,
    same_length(Crabs, Fuels),
    maplist(crab_and_X_pos(X), Fuels, Crabs, FuelCosts),
    sum(FuelCosts, #=, Cost),
    labeling([min(Cost)], [Cost]).

fuel_cost_2(Crabs, X, Cost) :-
    Cost #>= 0,
    same_length(Crabs, Fuels),
    maplist(crab_and_X_pos_2(X), Fuels, Crabs, FuelCosts),
    sum(FuelCosts, #=, Cost),
    labeling([min(Cost)], [Cost, X]).

solution_07_01(Cost) :-
    slurp(Line_, '07/input.txt'),
    nth1(1, Line_, Line),
    split_string(Line, ',', '', NumChars),
    maplist(atom_number, NumChars, Crabs),
    median(Crabs, XMedian),
    fuel_cost(Crabs, XMedian, Cost).

% 1159837 is too low.
% 98039527 (X = 486); this was correct.
% 98039615 is too high (X = 487)
% 98040439 gonna be too high (X = 485)
% 97947369 is too low
solution_07_02(Cost, X) :-
    slurp(Line_, '07/input.txt'),
    nth1(1, Line_, Line),
    split_string(Line, ',', '', NumChars),
    maplist(atom_number, NumChars, Crabs),
    mean(Crabs, Mean),
    X #= Mean,
    fuel_cost_2(Crabs, X, Cost).
