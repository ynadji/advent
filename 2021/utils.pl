:- use_module(library(clpfd)).
:- use_module(library(statistics)).
:- use_module(library(pio)).

% Voodoo I don't understand yet.
lines([])           --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

eos([], []).

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

% Copy/pasted from https://www.swi-prolog.org/pldoc/doc/_SWI_/library/lists.pl?show=src#clumped/2
% because my version of prolog is too old.
clumped(Items, Counts) :-
    clump(Items, Counts).

clump([], []).
clump([H|T0], [H-C|T]) :-
    ccount(T0, H, T1, 1, C),
    clump(T1, T).

ccount([H|T0], E, T, C0, C) :-
    E == H,
    !,
    C1 is C0+1,
    ccount(T0, E, T, C1, C).
ccount(List, _, List, C, C).
