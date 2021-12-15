/*

*/
['../prolog/utils'].

:- dynamic edge/2.
:- dynamic route/3.

big(Cave) :- string_upper(Cave, Upper), Cave = Upper.
small(Cave) :- \+ big(Cave).

% TODO: Try to do this without assertz/1? Take a look at this:
% https://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_15.html

path_(U, "end", Path, First, _) :-
    edge(U, "end"),
    Pred =.. [route, First, "end", Path],
    assertz(Pred).
    
path_(U, V, Path, First, SmallVisited) :-
    edge(U, X),
    big(X),
    path_(X, V, [X|Path], First, SmallVisited).

path_(U, V, Path, First, SmallVisited) :-
    edge(U, X),
    small(X),
    \+ member(X, SmallVisited),
    path_(X, V, [X|Path], First, [X|SmallVisited]).

path(U, V) :-
    path_(U, V, [], U, []).

% Traverse the graph and "return" the Path
connected(X,Y) :- edge(X,Y).
connected(X,Y) :- edge(Y,X).

path2(A,B,Path) :-
    travel(A,B,[A],Q),
    reverse(Q,Path).

travel(A,B,P,[B|P]) :-
    connected(A,B).
travel(A,B,Visited,Path) :-
    connected(A,C),
    C \== B,
    \+member(C,Visited),
    travel(C,B,[C|Visited],Path).
% end others' code

cleanup :-
    retractall(edge(_, _)),
    retractall(route(_, _, _)).

distinct_paths(Paths) :-
    findall(_, path("start", "end"), _),
    setof(Path, route("start", "end", Path), Paths).

split_dash(X, U, V) :-
    split_string(X, '-', '-', [U, V]).

assert_edges([], []).
% Order matters. You need to duplicate the non-start/end edges (so they are
% undirected), but duplicating the start/end edges causes duplicate edges in the
% above formulation. This is because you don't check nor add "small" or "end"
% into SmallVisited. A nice little hack is to ensure "start" is always in the
% left side of edge/2, and "end" is always in the right side.
assert_edges(Us, ["start"|Vs]) :- assert_edges(["start"|Vs], Us).
assert_edges(["end"|Us], Vs) :- assert_edges(Vs, ["end"|Us]).
assert_edges([U|Us], [V|Vs]) :-
    (member(U, ["start", "end"]) ; member(V, ["start", "end"])),
    Pred =.. [edge, U, V],
    assertz(Pred),
    assert_edges(Us, Vs).
assert_edges([U|Us], [V|Vs]) :-
    Pred1 =.. [edge, U, V],
    Pred2 =.. [edge, V, U],
    assertz(Pred1),
    assertz(Pred2),
    assert_edges(Us, Vs).

solve(Input, DistinctPaths) :-
    cleanup,
    slurp(Lines, Input),
    maplist(split_dash, Lines, Us, Vs),
    assert_edges(Us, Vs),
    distinct_paths(Paths),
    length(Paths, DistinctPaths).

solution_12_01(DistinctPaths) :-
    solve('12/input.txt', DistinctPaths).
