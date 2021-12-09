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

% $ cat input.txt| cut -d'|' -f2 | sed 's/^ //' | tr ' ' '\n' | grep -P '^(..|...|....|.......)$' | wc -l
solution_08_01(Count) :-
    slurp(Lines, '08/input.txt'),
    maplist(split_output, Lines, Outputs0),
    maplist(split_str, Outputs0, Outputs1),
    flatten(Outputs1, Outputs),
    include(length_2_3_4_7, Outputs, Uniques),
    length(Uniques, Count).

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
deindex(C, N) :- index(N, C).

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

ugh(L, Pred, Chars) :-
    maplist(index, Chars, Indices),
    ugh_(L, Variables, Indices),
    append([map], [Variables], TermList),
    Pred =.. TermList.

ugh_(L, [], []).
ugh_(L, [Term|Terms], [Index|Indices]) :-
    nth1(Index, L, Term),
    ugh_(L, Terms, Indices).

predicates(L, [], []).
predicates(L, [Pred|Preds], [Seg|Segments]) :-
    string_chars(Seg, Chars),
    ugh(L, Pred, Chars),
    predicates(L, Preds, Segments).

nth1L(L, Index, X) :- nth1(Index, L, X).

decode(L, BadSegment, Num) :-
    string_chars(BadSegment, Chars),
    maplist(index, Chars, BadNums),
    maplist(nth1L(L), BadNums, Nums),
    maplist(deindex, Nums, GoodChars),
    msort(GoodChars, GoodCharsSorted),
    string_chars(Seg, GoodCharsSorted),
    segments(Seg, Num).

% ["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"]
% ["cdfeb", "fcadb", "cdfeb", "cdbaf"]
%
% [["be", "cfbegad", "cbdgef", "fgaecd", "cgeb", "fdcge", "agebfd", "fecdb", "fabcd", "edb"],["edbfga", "begcd", "cbg", "gc", "gcadebf", "fbgde", "acbgfd", "abcde", "gfcbed", "gfec"],["fgaebd", "cg", "bdaec", "gdafb", "agbcfd", "gdcbef", "bgcad", "gfac", "gcb", "cdgabef"],["fbegcd", "cbd", "adcefb", "dageb", "afcb", "bc", "aefdc", "ecdab", "fgdeca", "fcdbega"],["aecbfdg", "fbg", "gf", "bafeg", "dbefa", "fcge", "gcbea", "fcaegb", "dgceab", "fcbdga"],["fgeab", "ca", "afcebg", "bdacfeg", "cfaedg", "gcfdb", "baec", "bfadeg", "bafgc", "acf"],["dbcfg", "fgd", "bdegcaf", "fgec", "aegbdf", "ecdfab", "fbedc", "dacgb", "gdcebf", "gf"],["bdfegc", "cbegaf", "gecbf", "dfcage", "bdacg", "ed", "bedf", "ced", "adcbefg", "gebcd"],["egadfb", "cdbfeg", "cegd", "fecab", "cgb", "gbdefca", "cg", "fgcdab", "egfdb", "bfceg"],["gcafb", "gcf", "dcaebfg", "ecagb", "gf", "abcdeg", "gaef", "cafbge", "fdbac", "fegbdc"]]
% [["fdgacbe", "cefdb", "cefbgd", "gcbe"],["fcgedb", "cgb", "dgebacf", "gc"],["cg", "cg", "fdcagb", "cbg"],["efabcd", "cedba", "gadfec", "cb"],["gecf", "egdcabf", "bgf", "bfgea"],["gebdcfa", "ecba", "ca", "fadegcb"],["cefg", "dcbef", "fcge", "gbcadfe"],["ed", "bcgafe", "cdgba", "cbgef"],["gbdfcae", "bgc", "cg", "cgb"],["fgae", "cfgab", "fg", "bagce"]]
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
