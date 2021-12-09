/*
This was a fun one and particularly suited to Prolog. But as is tradition in
Prolog world, while the actual solution was easy, everything around it was a
huge pain in the ass.

The meat of the algorithm is in the `map` clauses. While the letters for the
terms in the map clauses don't have meaning, they match the ones used in the
problem and the letters are mapped to integers so we can use clpfd. For
example, we know that two character long "segments" _must_ be CF, which
correspond to the ones digit. Since we're dealing with a jumbled up display,
the C and F will be represented by other atoms ("ab") in the example. The clause
`map([C, F])` applied here then means, 'a' and 'b' are one of 3 (c) or 6 (f),
but we don't know which yet. All the map clauses assign these constraints, and
after saying they should all be distinct, Prolog poops out the answer:

% This corresponds to ["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"]
?- map([A, B]), map([D, A, B]), map([E, A, F, B]), map([A, C, E, D, G, F, B]), map([C, D, F, B, E]), map([G, C, D, F, A]), map([F, B, C, A, D]), map([C, E, F, A, B, D]), map([C, D, F, G, E, B]), map([C, A, G, E, D, B]), all_distinct([A, B, C, D, E, F, G]), [A, B, C, D, E, F, G] = L.
A = 3,
B = 6,
D = 1,
E = 2,
F = 4,
C = 7,
G = 5,
L = [3, 6, 7, 1, 2, 4, 5]

This means the As are in fact Cs, Bs -> Fs, Ds -> As, etc.

Most important thing I learned was how to force a relationship between the
characters in the input string and the variables used in the predicate. Due to
my formulation, I wanted input 'a's to be bound to the A variable in the 1st
element of the solutions list, 'b's to B in the 2nd, and so on and so forth,
so after the solution was found, I knew that the 1st element of the solution
list would hold the correctly deduced value for A, the 2nd for B, etc. Part of
this was to keep the names in my head straight and make the code readable,
because the variables need to be constrained by every `map` clause. There's prob
a smarter way to do this but /shrugs.

I thought I had figured out something clever here with term_string/3 and
variable_names/1, but it didn't work as I intended:

?- term_string(Term1, 'map([A, B])', [variable_names(VNames)]), term_string(Term2, 'map([D, A, B])', [variable_names(VNames2)]).
Term1 = map([_A, _B]),
VNames = ['A'=_A, 'B'=_B],
Term2 = map([_C, _D, _E]),
VNames2 = ['D'=_C, 'A'=_D, 'B'=_E].

The hope was the variables in the terms would be shared (by name), but this
clearly shows they don't. My workaround was the solution i came up with in
map_and_decode/3 below. For each signal/output pair, we set and name the list
that will hold the solutions and name the variables according to convention.
These could be anything, what matters are the positions: A -> 1, B -> 2, etc.
terms_and_indices/3 builds the appropriate map/1 predicate and relates its
argument to the correct positions in `L`, the solution list.

Fun problems!
*/
['../prolog/utils'].

split_input(Line, Output) :-
    split_string(Line, '|', ' ', Fields),
    nth1(1, Fields, Output).

split_output(Line, Output) :-
    split_string(Line, '|', ' ', Fields),
    nth1(2, Fields, Output).

split_str(Line, Fields) :-
    split_string(Line, ' ', ' ', Fields).

length_2_3_4_7(X) :-
    string_length(X, 2) ; string_length(X, 3) ; string_length(X, 4) ; string_length(X, 7).

% Hard to beat unix and pipes bay bee
% $ cat input.txt| cut -d'|' -f2 | sed 's/^ //' | tr ' ' '\n' | grep -P '^(..|...|....|.......)$' | wc -l
solution_08_01(Count) :-
    slurp(Lines, '08/input.txt'),
    maplist(split_output, Lines, Outputs0),
    maplist(split_str, Outputs0, Outputs1),
    flatten(Outputs1, Outputs),
    include(length_2_3_4_7, Outputs, Uniques),
    length(Uniques, Count).

% Just some notes from figuring out the solution.
%
% Segment "diagram" for the digital clock digits.
%
%   aaaa
%  b    c
%  b    c
%   dddd
%  e    f
%  e    f
%   gggg
%
% a: 1
% b: 2
% c: 3
% d: 4
% e: 5
% f: 6
% g: 7
%
% [acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab] | cdfeb fcadb cdfeb cdbaf
% ["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"]
%
% a -> c -> 3
% b -> f -> 6
% c -> g -> 7
% d -> a -> 1
% e -> b -> 2
% f -> d -> 4
% g -> e -> 5
%
% 5 segs: 2, 3, 5
% 6 segs: 0, 6, 9

index('a', 1).
index('b', 2).
index('c', 3).
index('d', 4).
index('e', 5).
index('f', 6).
index('g', 7).
% So I can use maplist/3 in the other direction.
deindex(C, N) :- index(N, C).

% Map (sorted) segments to the digital clock digit the represent.
segments("abcefg",  '0').
segments("cf",      '1').
segments("acdeg",   '2').
segments("acdfg",   '3').
segments("bcdf",    '4').
segments("abdfg",   '5').
segments("abdefg",  '6').
segments("acf",     '7').
segments("abcdefg", '8').
segments("abcdfg",  '9').

% Build map/1 predicates for `Chars` such that they map (heh) to the right
% position in `L`.
terms_and_indices(L, Pred, Chars) :-
    maplist(index, Chars, Indices),
    terms_and_indices_(L, Variables, Indices),
    append([map], [Variables], TermList),
    Pred =.. TermList.

terms_and_indices_(L, [], []).
terms_and_indices_(L, [Term|Terms], [Index|Indices]) :-
    nth1(Index, L, Term),
    terms_and_indices_(L, Terms, Indices).

predicates(L, [], []).
predicates(L, [Pred|Preds], [Seg|Segments]) :-
    string_chars(Seg, Chars),
    terms_and_indices(L, Pred, Chars),
    predicates(L, Preds, Segments).

% So I can use maplist/3 to relate the scrambled numeric representation of the
% digits to the good ones through the solution `L`.
nth1L(L, Index, X) :- nth1(Index, L, X).

decode(L, BadSegment, Num) :-
    string_chars(BadSegment, Chars),
    maplist(index, Chars, BadNums),
    maplist(nth1L(L), BadNums, Nums),
    maplist(deindex, Nums, GoodChars),
    % We depend on order, so the new characters must be sorted.
    msort(GoodChars, GoodCharsSorted),
    string_chars(Seg, GoodCharsSorted),
    segments(Seg, Num).

% These correspond to the two examples provided in the problem statement.
%
% map_and_decode(["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"], ["cdfeb", "fcadb", "cdfeb", "cdbaf"], Example1Answer).
%
% maplist(map_and_decode, [["be", "cfbegad", "cbdgef", "fgaecd", "cgeb", "fdcge", "agebfd", "fecdb", "fabcd", "edb"],["edbfga", "begcd", "cbg", "gc", "gcadebf", "fbgde", "acbgfd", "abcde", "gfcbed", "gfec"],["fgaebd", "cg", "bdaec", "gdafb", "agbcfd", "gdcbef", "bgcad", "gfac", "gcb", "cdgabef"],["fbegcd", "cbd", "adcefb", "dageb", "afcb", "bc", "aefdc", "ecdab", "fgdeca", "fcdbega"],["aecbfdg", "fbg", "gf", "bafeg", "dbefa", "fcge", "gcbea", "fcaegb", "dgceab", "fcbdga"],["fgeab", "ca", "afcebg", "bdacfeg", "cfaedg", "gcfdb", "baec", "bfadeg", "bafgc", "acf"],["dbcfg", "fgd", "bdegcaf", "fgec", "aegbdf", "ecdfab", "fbedc", "dacgb", "gdcebf", "gf"],["bdfegc", "cbegaf", "gecbf", "dfcage", "bdacg", "ed", "bedf", "ced", "adcbefg", "gebcd"],["egadfb", "cdbfeg", "cegd", "fecab", "cgb", "gbdefca", "cg", "fgcdab", "egfdb", "bfceg"],["gcafb", "gcf", "dcaebfg", "ecagb", "gf", "abcdeg", "gaef", "cafbge", "fdbac", "fegbdc"]], [["fdgacbe", "cefdb", "cefbgd", "gcbe"],["fcgedb", "cgb", "dgebacf", "gc"],["cg", "cg", "fdcagb", "cbg"],["efabcd", "cedba", "gadfec", "cb"],["gecf", "egdcabf", "bgf", "bfgea"],["gebdcfa", "ecba", "ca", "fadegcb"],["cefg", "dcbef", "fcge", "gbcadfe"],["ed", "bcgafe", "cdgba", "cbgef"],["gbdfcae", "bgc", "cg", "cgb"],["fgae", "cfgab", "fg", "bagce"]], Nums), sum_list(Nums, Example2Answer).
map_and_decode(Signal, Output, Num) :-
    L = [A, B, C, D, E, F, G],
    predicates(L, Preds, Signal),
    maplist(call, Preds),
    all_distinct(L),
    maplist(decode(L), Output, CharNums),
    string_chars(NumStr, CharNums),
    atom_number(NumStr, Num).

% one
map([C, F]) :-
    [C, F] ins 3 \/ 6.

% seven
map([A, C, F]) :-
    [A, C, F] ins 1 \/ 3 \/ 6.

% four
map([B, C, D, F]) :-
    [B, C, D, F] ins 2..4 \/ 6.

% eight
map([A, B, C, D, E, F, G]) :-
    [A, B, C, D, E, F, G] ins 1..7.

% two
map([A, C, D, E, G]) :-
    [A, C, D, E, G] ins 1 \/ 3..5 \/ 7.
% three
map([A, C, D, F, G]) :-
    [A, C, D, F, G] ins 1 \/ 3..4 \/ 6..7.
% five
map([A, B, D, F, G]) :-
    [A, B, D, F, G] ins 1..2 \/ 4 \/ 6..7.

% zero
map([A, B, C, E, F, G]) :-
    [A, B, C, E, F, G] ins 1..3 \/ 5..7.
% six
map([A, B, D, E, F, G]) :-
    [A, B, D, E, F, G] ins 1..2 \/ 4..7.
% nine
map([A, B, C, D, F, G]) :-
    [A, B, C, D, F, G] ins 1..4 \/ 6..7.

solution_08_02(Sum) :-
    slurp(Lines, '08/input.txt'),
    maplist(split_output, Lines, Outputs0),
    maplist(split_input, Lines, Inputs0),
    maplist(split_str, Inputs0, Signals),
    maplist(split_str, Outputs0, Outputs),
    maplist(map_and_decode, Signals, Outputs, Nums),
    sum_list(Nums, Sum).
