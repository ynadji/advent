['../prolog/utils'].

% count_increases([199,200,208,210,200,207,240,269,260,263], X).
% X = 7
count_increases([], 0).
count_increases([_], 0).
count_increases([X,Y|Rest], C) :-
    X < Y,
    count_increases([Y|Rest], CC),
    C is CC + 1.
count_increases([X,Y|Rest], C) :-
    X >= Y,
    count_increases([Y|Rest], CC),
    C is CC.

% solution1(X).
% X = 1688
solution1(X) :-
    numbers_from_file(Ns, 'input.txt'),
    count_increases(Ns, X).

% count_increases_3sum([199,200,208,210,200,207,240,269,260,263], X).
% X = 5
count_increases_3sum([], 0).
count_increases_3sum([_,_,_], 0).
count_increases_3sum([W,X,Y,Z|Rest], C) :-
    W + X + Y < X + Y + Z,
    count_increases_3sum([X,Y,Z|Rest], CC),
    C is CC + 1.
count_increases_3sum([W,X,Y,Z|Rest], C) :-
    W + X + Y >= X + Y + Z,
    count_increases_3sum([X,Y,Z|Rest], CC),
    C is CC.

% solution2(X).
% X = 1728
solution2(X) :-
    numbers_from_file(Ns, 'input.txt'),
    count_increases_3sum(Ns, X).
