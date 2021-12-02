['../prolog/utils'].

process_lines([], []).
process_lines([Line|Lines], [[Cmd,Mag]|Rest]) :-
    split_string(Line, ' ', ' ', [Cmd,MagStr]),
    atom_number(MagStr, Mag),
    process_lines(Lines, Rest).

read_input(CaM, File) :-
    slurp(As, File),
    process_lines(As, CaM).

x_and_y([], 0, 0).
x_and_y([["forward", Mag]|Rest], X, Y) :-
    x_and_y(Rest, XX, Y),
    X is XX + Mag.
x_and_y([["down", Mag]|Rest], X, Y) :-
    x_and_y(Rest, X, YY),
    Y is YY + Mag.
x_and_y([["up", Mag]|Rest], X, Y) :-
    x_and_y(Rest, X, YY),
    Y is YY - Mag.

% solution_02_01(Prod).
% Prod = 2073315
solution_02_01(Prod) :-
    read_input(CaM, 'input.txt'),
    x_and_y(CaM, X, Y),
    Prod is X * Y.

x_and_y_with_aim([], 0, 0, 0).
x_and_y_with_aim([["forward", Mag]|Rest], X, Y, Aim) :-
    x_and_y_with_aim(Rest, XX, YY, Aim),
    X is XX + Mag,
    Y is YY + (Mag * Aim).
% This predicate had to come before the "down" one, otherwise I would get
% the dreaded `false.` No clue why.
x_and_y_with_aim([["up", Mag]|Rest], X, Y, Aim) :-
    x_and_y_with_aim(Rest, X, Y, NewAim),
    Aim is NewAim - Mag.
x_and_y_with_aim([["down", Mag]|Rest], X, Y, Aim) :-
    x_and_y_with_aim(Rest, X, Y, NewAim),
    Aim is NewAim + Mag.

% solution_02_02(Prod).
% Prod = 1840311528
solution_02_02(Prod) :-
    read_input(CaM, 'input.txt'),
    % you needed to reverse this list, otherwise the update to Aim happened
    % _before_ subsequent products. this is uhh an ugly solution.
    reverse(CaM, MaC),
    x_and_y_with_aim(MaC, X, Y, _),
    Prod is X * Y.
