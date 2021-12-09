['../prolog/utils'].

% [[2,1,9,9,9,4,3,2,1,0], [3,9,8,7,8,9,4,9,2,1], [9,8,5,6,7,8,9,8,9,2], [8,7,6,7,8,9,6,7,8,9], [9,8,9,9,9,6,5,6,7,8]]
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
