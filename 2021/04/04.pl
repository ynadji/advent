['../prolog/utils'].

read_input(Nums, Boards) :-
    slurp([NumLine|Lines], 'input.txt'),
    split_string(NumLine, ',', '', NumChars),
    maplist(atom_number, NumChars, Nums),
    exclude(=(''), Lines, CleanLines),
    maplist(split_string_list, CleanLines, BoardRows),
    split_list(BoardRows, 5, Boards).

% [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
%
% [7,4,9,5,11,17,23,2,0,14,21,24,10,16]
% [[22,13,17,11, 0], [8, 2,23, 4,24], [21, 9,14,16, 7], [6,10, 3,18, 5], [1,12,20,15,19]]
%
% [7,4,9,5,11,17,23,2,0,14,21,24]
% [[14, 21, 17, 24,  4], [10, 16, 15,  9, 19], [18,  8, 23, 26, 20], [22, 11, 13,  6,  5], [2,  0, 12,  3,  7]]
%
% [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13]
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

new_board(Nums, _, Board) :- board(Nums, Board, _).

/*
Very inefficient solution, but it was correct for the short example. We'll see
if it finishes by the time I get back from my bike ride.

The rough approach is as follows:
* For each subset of Nums,
* Exclude all boards that succeed,
* Until there is exactly one board (the last board to win),
* Find the solution/score for the last board.

It's slow af for a couple of reasons:
* We recompute Try when we might as well at least start from the point where it
was the only unsuccessful goal.
* We try increasing, rather than decreasing, subsets of Nums. If we go the other
way, we will probably succeed more quickly.
* The set stuff is very inefficient according to the docs.

reverse([1,2,3,4,5,6,7], L), append(Try, Other, L), reverse(Try, TryR), reverse(Other, OtherR).

* can i do an include() to identify which boards already pass, so i can ignore them on future runs?
* https://www.swi-prolog.org/pldoc/doc/_SWI_/library/ordsets.pl
  * made some changes. everything is right _except_ the score...

uhh all of a sudden it's fast now?? wtf?? so things were slow. i implemented the
reversed_append where instead of checking Nums[:1], Nums[:2], ... i check Nums[:-1],
Nums[:-2], ... since i figured it was more likely the last solvable one would use closer
to most of the Nums. still slow af. tried to use ordsets, but was getting incorrect
answers so i stashed those changes. added the profiler, profiled the small set,
all normal. profiled the long running solution, immediately pooped out an answer!
WHAT??

i hate not understanding things D:<
*/
solution_04_02__(Nums, Boards, LastBoard) :-
    reverse(Nums, NumsR),
    append(_, TryR, NumsR),
    reverse(TryR, Try),
    exclude(new_board(Try, _), Boards, Remaining),
    length(Remaining, 1),
    nth1(1, Remaining, LastBoard).

% for example
% solution_04_02(Score).
% Score = 1924
% solution_04_02(Score).
% Score = 6804
solution_04_02(Score) :-
    read_input(Nums, Boards),
    %Nums = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1],
    %Boards = [[[22,13,17,11, 0], [8, 2,23, 4,24], [21, 9,14,16, 7], [6,10, 3,18, 5], [1,12,20,15,19]], [[14, 21, 17, 24,  4], [10, 16, 15,  9, 19], [18,  8, 23, 26, 20], [22, 11, 13,  6,  5], [2,  0, 12,  3,  7]], [[3, 15,  0,  2, 22], [9, 18, 13, 17,  5], [19,  8,  7, 25, 23], [20, 11, 10, 24,  4], [14, 21, 16, 12,  6]]],
    solution_04_02__(Nums, Boards, LastBoard),
    append(Try, _, Nums),
    board(Try, LastBoard, Score).
