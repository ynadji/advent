:- use_module(library(clpfd)).
:- use_module(library(statistics)).
:- use_module(library(pio)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(lambda)).

/*
Haven't figured out how to do this appropriately. I do a lot of needless
conversions between codes/chars/strings. It would be nice to just treat the
strings as a list of characters as described in https://www.metalevel.at/prolog/data
Various things seem to break but I'll leave commented out bits to describe my
current half solutions.
*/
%:- set_prolog_flag(double_quotes, chars).
% Opposite of above.
%:- set_prolog_flag(double_quotes, codes).
% This actually seems to be the one that makes solution_08_02 not fail.
%:- set_prolog_flag(double_quotes, string).

% Voodoo I don't understand yet. Stolen from https://stackoverflow.com/a/4805709/5586983
lines([])           --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

eos([], []).

% [10] means "\n" here. Kinda works for :- set_prolog_flag(double_quotes, chars).
%
% line([])     --> ( [10] ; call(eos) ), !.
% [10] means "\n" here
line([])     --> ( "\n" ; call(eos) ), !.
line([L|Ls]) -->
    [L], line(Ls).

lines_to_atoms([], []).
lines_to_atoms([HL|Ls], [A|As]) :-
    atom_chars(A, HL),
    lines_to_atoms(Ls, As).

slurp(As, File) :-
    phrase_from_file(lines(Ls), File),
    lines_to_atoms(Ls, As).

lines_to_numbers([], []).
lines_to_numbers([HL|Ls], [HN|Ns]) :-
    atom_chars(X, HL),
    atom_number(X, HN),
    lines_to_numbers(Ls, Ns).

numbers_from_file(Ns, File) :-
    phrase_from_file(lines(Ls), File),
    lines_to_numbers(Ls, Ns).

split_string_list(Line, Row) :-
    split_string(Line, ' ', ' ', NumChars),
    maplist(atom_number, NumChars, Row).

% Nifty trick to change the order of arguments for a predicate so it can be used
% with maplist
list_length(Size, List) :- length(List, Size).

% This will search forever for a second solution.
% Stolen from: https://stackoverflow.com/a/46622087
split_list(List, SubSize, SubLists) :-
    maplist(list_length(SubSize), SubLists),
    append(SubLists, List).
