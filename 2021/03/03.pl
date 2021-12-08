['../prolog/utils'].

to_nums(Chars, Nums) :-
    maplist(atom_number, Chars, Nums).

read_input(Nums, File) :-
    slurp(As, File),
    maplist(atom_chars, As, Cs),
    maplist(to_nums, Cs, Nums).

arr_sum([], [], []).
arr_sum([X|Xs], [Y|Ys], [S|Sum]) :-
    arr_sum(Xs, Ys, Sum),
    S is X + Y.

div_z(Z, X, Y) :- Y is X / Z.

bit(X, 1) :-
    X >= 0.5.
bit(_, 0).
tib(X, 1) :-
    X < 0.5.
tib(_, 0).

bin2int([], _, 0).
bin2int([B|Rest], Next, Int) :-
    succ(Next, NextNext),
    bin2int(Rest, NextNext, NewInt),
    Int is NewInt + B * 2^Next.

most_and_least_common_bits(Nums, Most, Least) :-
    length(Nums, Len),
    foldl(arr_sum, Nums, [0,0,0,0,0,0,0,0,0,0,0,0], Sums),
    maplist(div_z(Len), Sums, Prop),
    maplist(bit, Prop, Most),
    maplist(tib, Prop, Least).

solution_03_01(Foo) :-
    read_input(Nums, '03/input.txt'),
    most_and_least_common_bits(Nums, Most, Least),
    reverse(Most, Bits),
    reverse(Least, Stib),
    bin2int(Bits, 0, Gamma),
    bin2int(Stib, 0, Epsilon),
    Foo is Gamma * Epsilon.

app(X, [], [X]).
app(X, [Y | S], [Y | S2]) :- app(X, S, S2).

% read_input(Nums, 'input.txt'),
% iter_bit_filter(Nums, Pre).

%iter_bit_filter([Ans], _).
%iter_bit_filter(Nums, MostPre) :-
%    include(prefix(MostPre), Nums, NumsMostFiltered),
%    most_and_least_common_bits(NumsMostFiltered, Most, _),
%    length(MostPre, Len),
%    nth0(Len, Most, NewBit),
%    app(NewBit, MostPre, NewMostPre),
%    iter_bit_filter(NumsMostFiltered, NewMostPre).

most_common(Nums, Idx, Bit) :-
    maplist(nth0(Idx), Nums, NBits),
    foldl(plus, NBits, 0, Sum),
    length(Nums, Len),
    ( Sum / Len >= 0.5
                     -> Bit = 1
                     ;  Bit = 0
    ).
least_common(Nums, Idx, Bit) :-
    maplist(nth0(Idx), Nums, NBits),
    foldl(plus, NBits, 0, Sum),
    length(Nums, Len),
    ( Sum / Len >= 0.5
                     -> Bit = 0
                     ;  Bit = 1
    ).

% [[0,0,1,0,0],[1,1,1,1,0],[1,0,1,1,0],[1,0,1,1,1],[1,0,1,0,1],[0,1,1,1,1],[0,0,1,1,1],[1,1,1,0,0],[1,0,0,0,0],[1,1,0,0,1],[0,0,0,1,0],[0,1,0,1,0]]

again([Bits], _, Oxygen, _) :-
    reverse(Bits, Stib),
    bin2int(Stib, 0, Oxygen).
again(Nums, [Bit|Rest], Oxygen, Idx) :-
    most_common(Nums, Idx, Bit),
    include(prefix([Bit|Rest]), Nums, NewNums),
    succ(Idx, NewIdx),
    again(NewNums, [Bit|Rest], Oxygen, NewIdx).

solve(Nums, Bits, Oxygen) :-
    Bits = [X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12],
    most_common(Nums, 0, X1),
    include(prefix([X1]), Nums, Nums1),
    most_common(Nums1, 1, X2),
    include(prefix([X1, X2]), Nums1, Nums2),
    most_common(Nums2, 2, X3),
    include(prefix([X1, X2, X3]), Nums2, Nums3),
    most_common(Nums3, 3, X4),
    include(prefix([X1, X2, X3, X4]), Nums3, Nums4),
    most_common(Nums4, 4, X5),
    include(prefix([X1, X2, X3, X4, X5]), Nums4, Nums5),
    most_common(Nums5, 5, X6),
    include(prefix([X1, X2, X3, X4, X5, X6]), Nums5, Nums6),
    most_common(Nums6, 6, X7),
    include(prefix([X1, X2, X3, X4, X5, X6, X7]), Nums6, Nums7),
    most_common(Nums7, 7, X8),
    include(prefix([X1, X2, X3, X4, X5, X6, X7, X8]), Nums7, Nums8),
    most_common(Nums8, 8, X9),
    include(prefix([X1, X2, X3, X4, X5, X6, X7, X8, X9]), Nums8, Nums9),
    most_common(Nums9, 9, X10),
    include(prefix([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10]), Nums9, Nums10),
    most_common(Nums10, 10, X11),
    include(prefix([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11]), Nums10, Nums11),
    most_common(Nums11, 11, X12),
    reverse(Bits, Stib),
    bin2int(Stib, 0, Oxygen).

solve2(Nums, Bits, CO2, Last) :-
    Bits = [X1, X2, X3, X4, X5, X6, X7, X8, X9],%, X10, X11],
    least_common(Nums, 0, X1),
    include(prefix([X1]), Nums, Nums1),
    least_common(Nums1, 1, X2),
    include(prefix([X1, X2]), Nums1, Nums2),
    least_common(Nums2, 2, X3),
    include(prefix([X1, X2, X3]), Nums2, Nums3),
    least_common(Nums3, 3, X4),
    include(prefix([X1, X2, X3, X4]), Nums3, Nums4),
    least_common(Nums4, 4, X5),
    include(prefix([X1, X2, X3, X4, X5]), Nums4, Nums5),
    least_common(Nums5, 5, X6),
    include(prefix([X1, X2, X3, X4, X5, X6]), Nums5, Nums6),
    least_common(Nums6, 6, X7),
    include(prefix([X1, X2, X3, X4, X5, X6, X7]), Nums6, Nums7),
    least_common(Nums7, 7, X8),
    include(prefix([X1, X2, X3, X4, X5, X6, X7, X8]), Nums7, Nums8),
    least_common(Nums8, 8, X9),
    include(prefix([X1, X2, X3, X4, X5, X6, X7, X8, X9]), Nums8, Nums9),
    %least_common(Nums9, 9, X10),
    %include(prefix([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10]), Nums9, Nums10),
    %least_common(Nums10, 10, X11),
    %include(prefix([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11]), Nums10, Nums11),
    %least_common(Nums11, 11, X12),
    nth0(0, Nums8, Last),
    reverse(Last, Stib),
    bin2int(Stib, 0, CO2).

solution_03_02(LifeSupport) :-
    read_input(Nums, '03/input.txt'),
    solve(Nums, _, Oxygen),
    solve2(Nums, _, CO2, Nums11),
    LifeSupport is Oxygen * CO2.
