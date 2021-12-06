['../prolog/utils'].

split_points(Line, Points) :-
    split_string(Line, ' ->,', ' ->,', Fields),
    maplist(atom_number, Fields, Points).

horizontal_or_vertical([X, _, X, _]).
horizontal_or_vertical([_, Y, _, Y]).

assert_points([_,[]]).
assert_points([[],_]).
assert_points([X,[Y|Rest]]) :-
    assert_points([X,Y]),
    assert_points([X,Rest]).
assert_points([[X|Rest],Y]) :-
    assert_points([X,Y]),
    assert_points([Rest,Y]).
assert_points(Points) :-
    append([covers], Points, TermList),
    Term =.. TermList,
    assertz(Term).

overlaps :-
    aggregate(count, covers(_, _), Count), Count #> 1.

all_between(X1, X2, Xs) :-
    (
        X2 > X1 ->
        findall(X, between(X1, X2, X), Xs)
    ;
    findall(X, between(X2, X1, X), Xs)
    ).

hov_line([X, Y, X, Y]) :- assert_points([X, Y]).
hov_line([X1,Y, X2,Y]) :-
    all_between(X1, X2, Xs),
    assert_points([Xs, Y]).
hov_line([X, Y1, X, Y2]) :-
    all_between(Y1, Y2, Ys),
    assert_points([X, Ys]).
hov_lines([]).
hov_lines([Points|Rest]) :-
    hov_line(Points),
    hov_lines(Rest).

solution_05_01(Count, HOVPoints) :-
    % read the stuff and make assertions
    slurp(Lines, '/home/yacin/code/advent/2021/05/input.txt'),
    maplist(split_points, Lines, PointsList),
    include(horizontal_or_vertical, PointsList, HOVPoints),
    hov_lines(HOVPoints),
    aggregate_all(count, overlaps, Count).
