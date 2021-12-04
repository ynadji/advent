:- use_module(library(clpfd)).
:- use_module(library(csv)).

['../prolog/utils'].

read_input(Nums, Boards) :-
    slurp([NumLine|Lines], 'input.txt'),
    split_string(NumLine, ',', '', NumChars),
    maplist(atom_number, NumChars, Nums),
    exclude(=(''), Lines, CleanLines),
    maplist(split_string_list, CleanLines, BoardRows),
    split_list(BoardRows, 5, Boards).

% [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
% [[22,13,17,11, 0], [8, 2,23, 4,24], [21, 9,14,16, 7], [6,10, 3,18, 5], [1,12,20,15,19]]
% [[14, 21, 17, 24,  4], [10, 16, 15,  9, 19], [18,  8, 23, 26, 20], [22, 11, 13,  6,  5], [2,  0, 12,  3,  7]]
% [[3, 15,  0,  2, 22], [9, 18, 13, 17,  5], [19,  8,  7, 25, 23], [20, 11, 10, 24,  4], [14, 21, 16, 12,  6]]
% [[[22,13,17,11, 0], [8, 2,23, 4,24], [21, 9,14,16, 7], [6,10, 3,18, 5], [1,12,20,15,19]], [[14, 21, 17, 24,  4], [10, 16, 15,  9, 19], [18,  8, 23, 26, 20], [22, 11, 13,  6,  5], [2,  0, 12,  3,  7]], [[3, 15,  0,  2, 22], [9, 18, 13, 17,  5], [19,  8,  7, 25, 23], [20, 11, 10, 24,  4], [14, 21, 16, 12,  6]]]
    
board(CalledNums, Board, Score) :-
    length(Board, 5),
    maplist(same_length(Board), Board),
    % These two might be wrong/unneeded.
    flatten(Board, FlatBoard),
    all_distinct(FlatBoard),
    nth1(1, Board, Row1),
    nth1(2, Board, Row2),
    nth1(3, Board, Row3),
    nth1(4, Board, Row4),
    nth1(5, Board, Row5),
    transpose(Board, BoardT),
    nth1(1, BoardT, Col1),
    nth1(2, BoardT, Col2),
    nth1(3, BoardT, Col3),
    nth1(4, BoardT, Col4),
    nth1(5, BoardT, Col5),
    (
    intersection(Row1, CalledNums, Row1)
    ;
    intersection(Row2, CalledNums, Row2)
    ;
    intersection(Row3, CalledNums, Row3)
    ;
    intersection(Row4, CalledNums, Row4)
    ;
    intersection(Row5, CalledNums, Row5)
    ;
    intersection(Col1, CalledNums, Col1)
    ;
    intersection(Col2, CalledNums, Col2)
    ;
    intersection(Col3, CalledNums, Col3)
    ;
    intersection(Col4, CalledNums, Col4)
    ;
    intersection(Col5, CalledNums, Col5)
    ),
    subtract(FlatBoard, CalledNums, UnmarkedNums),
    sum_list(UnmarkedNums, UnmarkedSum),
    last(CalledNums, JustCalled),
    Score is UnmarkedSum * JustCalled.

solution_04_01(Score) :-
    read_input(Nums, Boards),
    solution_04_01__(Nums, Boards, Score).

solution_04_01_(Nums, [Board|Boards], Score) :-
    board(Nums, Board, Score)
    ;
    solution_04_01_(Nums, Boards, Score).

solution_04_01__(Nums, Boards, Score) :-
    % I think this is the trick!
    append(Try, _, Nums),
    solution_04_01_(Try, Boards, Score).
