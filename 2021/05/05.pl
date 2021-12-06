/*
For each point (X, Y) we add the fact `covers(X, Y)` to the DB (using `assertz`).
To get the solution, we simply ask the DB how many times we have `covers(_, _)`
that occur more than once (see `overlaps`). Some interesting things I learned:

- originally I was just using `between(X1, X2, X) ; between(X2, X1, X)` inside
`draw_line`, but then only the first solution would go through. When I tried
doing `findall` in the solution clause, it blew out the stack.
- `assertz` is awesome! Generating facts from strings is quite powerful.
- My solution has multiple results (and they increase) and I don't know why :(.

?- solution_05_02(Count).
Count = 17882 ;
Count = 17882 ;
Count = 17882 ;
Count = 17883 ;
Count = 17883

*/
['../prolog/utils'].

split_points(Line, Points) :-
    split_string(Line, ' ->,', ' ->,', Fields),
    maplist(atom_number, Fields, Points).

horizontal_or_vertical([X, _, X, _]).
horizontal_or_vertical([_, Y, _, Y]).

assert_points([_,[]]).
assert_points([[],_]).
assert_points([[X|XRest],[Y|YRest]]) :-
    assert_points([X,Y]),
    assert_points([XRest,YRest]).
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
    findall(X, between(X2, X1, X), Xs_),
    reverse(Xs_, Xs)
    ).

draw_line([X, Y, X, Y]) :- assert_points([X, Y]).
draw_line([X1,Y, X2,Y]) :-
    all_between(X1, X2, Xs),
    assert_points([Xs, Y]).
draw_line([X, Y1, X, Y2]) :-
    all_between(Y1, Y2, Ys),
    assert_points([X, Ys]).
draw_line([X1, Y1, X2, Y2]) :-
    all_between(X1, X2, Xs),
    all_between(Y1, Y2, Ys),
    assert_points([Xs, Ys]).

draw_lines([]).
draw_lines([Points|Rest]) :-
    draw_line(Points), !,
    draw_lines(Rest).

solution_05_01(Count) :-
    slurp(Lines, '05/input.txt'),
    maplist(split_points, Lines, PointsList),
    include(horizontal_or_vertical, PointsList, HOVPoints),
    draw_lines(HOVPoints),
    aggregate_all(count, overlaps, Count).

solution_05_02(Count) :-
    slurp(Lines, '05/input.txt'),
    maplist(split_points, Lines, PointsList),
    draw_lines(PointsList),
    aggregate_all(count, overlaps, Count).
