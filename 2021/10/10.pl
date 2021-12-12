/*
So this one was tricky. After reading the prompt, I felt pretty smart when I
realized how simple the code was to check if the parens are balanced:

inefficient_balanced([]).
inefficient_balanced(Chunk) :-
    append([Front, [O, C], Back], Chunk),
    open_close(O, C),
    append(Front, Back, Rest),
    inefficient_balanced(Rest).

I used this instead of the more complicated balanced/3 I have implemented below
initially, and it was unable to find the first solution after _9 hours_:

?- profile(solution_10_01(ErrorScore)).
  C-c C-cAction (h for help) ? a
abort
=====================================================================
Total time: 32542.524 seconds
=====================================================================
Predicate                       Box Entries =    Calls+Redos     Time
=====================================================================
append/3                    217,667,234,846 =43,465,245,857+174,201,988,989 62.2%
lists:append_/2              20,553,740,684 =1,178,882,255+19,374,858,429 27.1%
balanced/1                   38,749,716,851 =        1+38,749,716,850 2.5%
closed/1                     17,350,163,868 =17,350,163,868+0         2.1%
open_close/2                 20,553,740,674 =20,553,740,674+0         2.0%
mismatched_open_close/2      19,374,858,421 =19,374,858,420+1         1.9%
open/1                       19,374,858,420 =19,374,858,420+0         1.8%
error:has_type/2              1,178,882,255 =1,178,882,255+0         0.2%
must_be/2                     1,178,882,255 =1,178,882,255+0         0.1%
append/2                     20,553,740,684 =1,178,882,255+19,374,858,429 0.1%
is_list/1                     1,178,882,255 =1,178,882,255+0         0.1%
$wakeup/1                                11 =       11+0         0.0%
$attvar:call_all_attr_uhooks/2           11 =       11+0         0.0%
$attvar:uhook/3                          11 =       11+0         0.0%
$dcg:dcg_special/1                        1 =        1+0         0.0%
$dcg:phrase_input/1                       2 =        2+0         0.0%
$messages:insert_prefix/4                 1 =        1+0         0.0%
$messages:line_element/2                 10 =        5+5         0.0%
$messages:maybe_halt_on_error/1           1 =        1+0         0.0%
$messages:msg_prefix/2                    1 =        1+0         0.0%
$messages:msg_property/2                  5 =        2+3         0.0%
$messages:must_print/2                    2 =        1+1         0.0%
$messages:pop_msg/0                       1 =        1+0         0.0%
$messages:prefix_nl/4                     1 =        1+0         0.0%
print_message/2                          11 =        1+10        0.0%
% Execution Aborted

Look at how much time was spent trying different values for append/3! I
understand the warning about append in the SWI-Prolog docs a little bit more.

The most annoying part about this problem (and #9 which I have yet to complete),
is that a particular condition _failing_ causes branching behavior. To make
matters worse, there are two slightly different ways to fail:

1.) wrong closing bracket (AKA corrupted), and
2.) extra opening brackets (AKA incomplete).

I don't know how to find these and deterministically "send" data back to the
caller. Instead, I did a slightly un-Prolog-y thing and:

- identify these cases explicitly in the recursion for balanced_/3
- use assertz/1 to store the intermediate data
- fail to return control to the caller
- retrieve the data in caller
- do necessary computation
- clean up in the called with retractall/1

We are manipulating global state now, so we've lost logical purity :(. But hey,
at least we solved the problem! Important things to remember are always clean up
your shit with retractall/1.

I don't know if there's a more "Prolog-y" way to solve these problems.
*/

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
% Find incomplete cases, assert the remaining brackets to close, and fail.
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
% Find corrupted cases, assert the first failure, and fail.
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
        % If it's balanced, go to the next Chunk
        solve(Chunks, ErrorScore) ;
        (
            first_illegal(Bracket) ->
            % If it isn't balanced _and_ it failed because of an open_close/2
            (
                error_score(Bracket, Points),
                ErrorScore #= Points + ErrorScore0,
                retractall(first_illegal(_)),
                solve(Chunks, ErrorScore0)
            ) ;
            % If it isn't balanced, not open_close/2, and there are extra Opens
            % left to_close/1.
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
