/*
This one is a fun Prolog-y take on a relatively straightforward search problem.
Rather than search the matrix directly, we use the matrix's composition to
assert relations between its indices and values. To handle out of bounds issues,
imagine we place 10s (greater than the largest value in the matrix) around the
entire matrix to low points on the edges are correctly found. This only requires
four clauses:

val(0, _, 10).
val(_, 0, 10).
val(NumRows + 1, _, 10).
val(0, NumCols + 1, 10).

Well it was fun until Part 2. Kinda boxed myself into a corner here and I don't
know what to try next. Lots of problems here and it's time to throw in the towel
and maybe come back to this guy later. You need to figure out how to do a DFS or
BFS search in Prolog. Normally you end with the empty list, and the elements
build back up. In this case, the terminating condition isn't the empty list,
it's like "you've reached the boundary of the matrix" or "you've seen all the
nodes in this graph" so you can't easily rebuild the path. I have seen examples
[1] where the solution up to that point is simply assertz'd and retrieved later,
but that seems clunky to me. Especially considering that in this use case, we
need to combine the solution of four recursions at each step (U, D, L, R).
Perhaps an approach is:

- Identify all low points
- DFS from low point
- On termination, assert your low point root and path
- Union paths per root, removing duplicates

But this seems uhh really clunky. Surely there's a better way?

[1] http://rlgomes.github.io/work/prolog/2012/05/22/19.00-prolog-and-graphs.html
*/
['../prolog/utils'].

% [[2,1,9,9,9,4,3,2,1,0], [3,9,8,7,8,9,4,9,2,1], [9,8,5,6,7,8,9,8,9,2], [8,7,6,7,8,9,6,7,8,9], [9,8,9,9,9,6,5,6,7,8]]
% retractall(val(_,_,_)).
% findall(_, populate_values([[2,1,9,9,9,4,3,2,1,0], [3,9,8,7,8,9,4,9,2,1], [9,8,5,6,7,8,9,8,9,2], [8,7,6,7,8,9,6,7,8,9], [9,8,9,9,9,6,5,6,7,8]]), _).
%
% [[2,1,9,9,9,4,3,2,1,0],
%  [3,9,8,7,8,9,4,9,2,1],
%  [9,8,5,6,7,8,9,8,9,2],
%  [8,7,6,7,8,9,6,7,8,9],
%  [9,8,9,9,9,6,5,6,7,8]]

% 2199943210
% 3987894921
% 9856789892
% 8767896789
% 9899965678
%
% 0 2 3
% 4 5 6
% 7 8 2

:- dynamic val/3.
% Top edge
val(0, _, 10).
% Left edge
val(_, 0, 10).

populate_values(A) :-
    length(A, NumRows),
    maplist(length, A, Lengths),
    sort(Lengths, [NumCols]),
    OOBbottom #= NumRows + 1,
    OOBright #= NumCols + 1,
    RightEdge  =.. [val, _, OOBright, 10],
    BottomEdge =.. [val, OOBbottom, _, 10],
    assertz(RightEdge),
    assertz(BottomEdge),
    nth1(I, A, Row),
    nth1(J, Row, X),
    MatrixVal =.. [val, I, J, X],
    assertz(MatrixVal).

low(X, U, D, L, R) :-
    U #> X,
    D #> X,
    L #> X,
    R #> X,

    Iminus #= I - 1, % row above
    Jminus #= J - 1, % col left
    Iplus  #= I + 1, % row below
    Jplus  #= J + 1, % col right

    val(I, J, X),
    val(Iminus, J, U),
    val(I, Jminus, L),
    val(Iplus, J, D),
    val(I, Jplus, R).

low(X, [U, D, L, R]) :-
    U #> X,
    D #> X,
    L #> X,
    R #> X,

    Iminus #= I - 1, % row above
    Jminus #= J - 1, % col left
    Iplus  #= I + 1, % row below
    Jplus  #= J + 1, % col right

    val(I, J, X),
    if_( U = 9, true, val(Iminus, J, U)),
    if_( D = 9, true, val(Iplus, J, D)),
    if_( L = 9, true, val(I, Jminus, L)),
    if_( R = 9, true, val(I, Jplus, R)).

%basin([], _, _, _, _). % ?
%basin(9, _, _, _, _, Xs).
%basin(10, _, _, _, _, Xs).
basin(X, U, D, L, R, Visited) :-
    val(I, J, X),
    member([I,J], Visited),
    low(X, [U, D, L, R]),
    basin(U, _, 9, _, _, [[I,J]|Visited]), % D will be X and should be ignored
    basin(D, 9, _, _, _, [[I,J]|Visited]), % U will be X and should be ignored
    basin(L, _, _, _, 9, [[I,J]|Visited]), % R will be X and should be ignored
    basin(R, _, _, 9, _, [[I,J]|Visited]). % L will be X and should be ignored

neighbors(X, I, J, U, D, L, R) :-
    val(I, J, X),

    Iminus #= I - 1, % row above
    Jminus #= J - 1, % col left
    Iplus  #= I + 1, % row below
    Jplus  #= J + 1, % col right

    val(I, J, X),
    val(Iminus, J, U),
    val(I, Jminus, L),
    val(Iplus, J, D),
    val(I, Jplus, R).

% This is close i think
dfs(0, _, Visited, Vals).
dfs(I, J, Visited, Vals) :-
    val(I, J, X),
    Iminus #= I - 1, % row above
    dfs(Iminus, J, [[I,J]|Visited], [X|Vals]),
    Jminus #= J - 1, % col left
    Iplus  #= I + 1, % row below
    Jplus  #= J + 1. % col right

parse_line(Line, Nums) :-
    string_chars(Line, Chars),
    maplist(atom_number, Chars, Nums).
    
solution(RiskLevel) :-
    slurp(Lines, '09/input.txt'),
    maplist(parse_line, Lines, A),
    findall(_, populate_values(A), _),
    findall(X, low(X, _, _, _, _), Lows),
    maplist(plus(1), Lows, Risks),
    sum_list(Risks, RiskLevel).
