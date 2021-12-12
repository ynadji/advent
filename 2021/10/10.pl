['../prolog/utils'].


open('(').
open('[').
open('{').
open('<').

closed(')').
closed(']').
closed('}').
closed('>').

open_close('(', ')').
open_close('[', ']').
open_close('{', '}').
open_close('<', '>').

error_score(')',     3).
error_score(']',    57).
error_score('}',  1197).
error_score('>', 25137).

autocomplete_score(')', 1).
autocomplete_score(']', 2).
autocomplete_score('}', 3).
autocomplete_score('>', 4).

mismatched_open_close(O, C) :-
    open(O),
    closed(C),
    \+ open_close(O, C).

:- dynamic first_illegal/1.
:- dynamic to_close/1.
:- dynamic ascore/1.
% ["[<>({}){}[([])<>]]", "{([(<{}[<>[]}>{[]{[(<()>", "[[<[([]))<([[{}[[()]]]", "[{[{({}]{}}([{[{{{}}([]", "[<(<(<(<{}))><([]([]()", "<{([([[(<>()){}]>(<<{{", "[<>({}){}[([])<>]]"]

% [ "[({(<(())[]>[[{[]{<()<>>", "[(()[<>])]({[<{<<[]>>(", "{([(<{}[<>[]}>{[]{[(<()>", "(((({<>}<{<{<>}{[]{[]{}", "[[<[([]))<([[{}[[()]]]", "[{[{({}]{}}([{[{{{}}([]", "{<[[]]>}<{[{[{[]{()[[[]", "[<(<(<(<{}))><([]([]()", "<{([([[(<>()){}]>(<<{{", "<{([{{}}[<[[[<>{}]]]>[]]" ]

balanced_([], [], []).
balanced_([], ToClose, _) :-
    Pred =.. [to_close, ToClose],
    assertz(Pred), fail.
balanced_([O|Chunks], Opens, Closeds) :-
    open(O),
    balanced_(Chunks, [O|Opens], Closeds).
balanced_([C|Chunks], Opens, Closeds) :-
    closed(C),
    Opens = [O|Rest],
    open_close(O, C),
    balanced_(Chunks, Rest, Closeds).
balanced_([C|_], Opens, _) :-
    closed(C),
    nth1(1, Opens, O),
    mismatched_open_close(O, C),
    Pred =.. [first_illegal, C],
    assertz(Pred), fail.
balanced(Chunk) :-
    balanced_(Chunk, _, _).

comp_as(C, Acc, AS) :-
    autocomplete_score(C, X),
    AS #= Acc * 5 + X.

median(L, X) :-
    msort(L, LS),
    append([Start, [X], Back], LS),
    same_length(Start, Back).

solve([], 0).
solve([Chunk|Chunks], ErrorScore) :-
    (
        balanced(Chunk) ->
        solve(Chunks, ErrorScore) ;
        (
            first_illegal(Bracket) ->
            (
                error_score(Bracket, Points),
                ErrorScore #= Points + ErrorScore0,
                retractall(first_illegal(_)),
                solve(Chunks, ErrorScore0)
            ) ;
            (
                to_close(Opens),
                maplist(open_close, Opens, Closes),
                foldl(comp_as, Closes, 0, AScore),
                % There is probably a better way to do this but I just want to
                % move on.
                Pred =.. [ascore, AScore],
                assertz(Pred),
                retractall(to_close(_)),
                solve(Chunks, ErrorScore)
            )
        )
    ).

cleanup :-
    retractall(first_illegal(_)),
    retractall(to_close(_)),
    retractall(ascore(_)).

solution_10(ErrorScore, AutocompleteScore) :-
    slurp(Lines, '10/input.txt'),
    maplist(string_chars, Lines, Chunks),
    solve(Chunks, ErrorScore),
    findall(X, ascore(X), AScores),
    median(AScores, AutocompleteScore),
    cleanup.
