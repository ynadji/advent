['../prolog/utils'].

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

