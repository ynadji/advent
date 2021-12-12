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

points(')',     3).
points(']',    57).
points('}',  1197).
points('>', 25137).

% These aren't right. This definition: https://www.reddit.com/r/adventofcode/comments/rd1s8l/2021_day_10_the_problem_is_confusing/hnyiq8y/
% seems useful.
% 
complete(Chunk) :-
    partition(open, Chunk, Opens, Closes),
    same_length(Opens, Closes).
incomplete(Chunk) :- \+ complete(Chunk).

mismatched_open_close(O, C) :-
    open(O),
    closed(C),
    \+ open_close(O, C).

chunk([]).
chunk([O,C]) :-
    open_close(O, C).

% This might work! Well, it might not help me find the first incorrect closing
% character :|. You might just need a stack tbh.
chunk(L) :-
    append([[O], Rest, [C]], L),
    open_close(O, C),
    chunk(Rest).
chunk(L) :-
    append([[O,C], Rest], L),
    open_close(O, C),
    chunk(Rest).
chunk(L) :-
    append([[O], Rest0, [C], Rest1], L),
    open_close(O, C),
    chunk(Rest0),
    chunk(Rest1).

string_chunk(S) :-
    string_chars(S, Chunk),
    chunk(Chunk).

%smember(X,[X|_]).
%smember(X,[_|T]) :- smember(X,T).

%empty_stack([]).

% member_stack tests if an element is a member of a stack

%member_stack(E, S) :- smember(E, S).

% stack performs the push, pop and peek operations
% to push an element onto the stack
% ?- stack(a, [b,c,d], S).
%    S = [a,b,c,d]
% To pop an element from the stack
% ?- stack(Top, Rest, [a,b,c]).
%    Top = a, Rest = [b,c]
% To peek at the top element on the stack
% ?- stack(Top, _, [a,b,c]).
%    Top = a

stack(E, S, [E|S]).

sschunk(S) :-
    string_chars(S, Chunk),
    schunk(Chunk).

wrong_close([X|Chunk], OS, CS) :-
    open(X).

:- dynamic first_illegal/1.
balanced([]).
balanced(Chunk) :-
    append([Front, [O, C], Back], Chunk),
    (
        open_close(O, C);
        % This happens multiple times, but I can always just use the first value
        % from first_illegal/1. Unsure if this will cause problems.
        (mismatched_open_close(O, C), Pred =.. [first_illegal, C], assertz(Pred))
    ),
    append(Front, Back, Rest),
    balanced(Rest).

% ["[<>({}){}[([])<>]]", "{([(<{}[<>[]}>{[]{[(<()>", "[[<[([]))<([[{}[[()]]]", "[{[{({}]{}}([{[{{{}}([]", "[<(<(<(<{}))><([]([]()", "<{([([[(<>()){}]>(<<{{", "[<>({}){}[([])<>]]"]

% [ "[({(<(())[]>[[{[]{<()<>>", "[(()[<>])]({[<{<<[]>>(", "{([(<{}[<>[]}>{[]{[(<()>", "(((({<>}<{<{<>}{[]{[]{}", "[[<[([]))<([[{}[[()]]]", "[{[{({}]{}}([{[{{{}}([]", "{<[[]]>}<{[{[{[]{()[[[]", "[<(<(<(<{}))><([]([]()", "<{([([[(<>()){}]>(<<{{", "<{([{{}}[<[[[<>{}]]]>[]]" ]

solve([], 0).
solve([Chunk|Chunks], ErrorScore) :-
    (
        balanced(Chunk) ->
        solve(Chunks, ErrorScore) ;
        (
            first_illegal(Bracket) ->
            (
                points(Bracket, Points),
                ErrorScore #= Points + ErrorScore0,
                retractall(first_illegal(_)),
                solve(Chunks, ErrorScore0)
            ) ;
            solve(Chunks, ErrorScore)
        )
    ).

% Seems too slow. There's not a way to identify incomplete vs. corrupted without
% doing all of balanced. I suspect you need to do this the more traditional way
% of having the stack and working your way down the list. I *think* you can do
% this with DCGs/difference lists. Here are some links:
% - https://stackoverflow.com/questions/31918227/difference-lists-in-prolog-and-mutable-variables
% - https://stackoverflow.com/questions/26966055/understanding-difference-lists/26967655#26967655
% - https://web.archive.org/web/20180923230230/http://homepages.inf.ed.ac.uk/pbrna/prologbook/node180.html
% - https://en.wikibooks.org/wiki/Prolog/Difference_Lists
%
% Maybe read this first: https://swi-prolog.discourse.group/t/six-ways-to-iterate-in-prolog/477
% This, I think, really clearly demonstrates the accumulator functionality of
% a difference list.
%
% FUcking annoying.
%solution_10_01(ErrorScore) :-
%    slurp(Lines, '10/input.txt'),
%    maplist(string_chars, Lines, Chunks),
%    solve(Chunks, ErrorScore).

english_spanish(a, 1).
english_spanish(b, 2).
english_spanish(c, 3).
english_spanish(d, 4).
english_spanish(e, 5).
english_spanish(f, 6).

translate_dl(EnglishList, SpanishList) :-
    translate_dl(EnglishList, Q-Q, SpanishList1),
    SpanishList-[] = SpanishList1.

translate_dl([], SpanishList, SpanishList).

translate_dl([English|EnglishList], Acc, SpanishList) :-
    english_spanish(English, Spanish),
    enqueue(Spanish, Acc, Acc1),
    translate_dl(EnglishList, Acc1, SpanishList).

enqueue(X, Qh-[X|Qt], Qh-Qt).

% This is the way. Difference lists baybee.
% Variables aren't named well, but there's a way to build a stack out of this
% yet. Well maybe. Things appear to get fucked up when I start tossing reverses
% into the equation.
imbalanced([], _, _).
imbalanced([X|Chunks], Acc, Result) :-
    open(X),
    enqueue(X, Acc, Acc0),
    imbalanced(Chunks, Acc0, Result).
imbalanced([C|Chunks], Acc, Result) :-
    closed(C),
    Acc = OpensRev-_,
    reverse(OpensRev, Opens),
    Opens = [O|RestR],
    open_close(O, C),
    reverse(RestR, Rest),
    imbalanced(Chunks, Rest-Qt, Result).

foo(L1, L2) :-
    foo(L1, Q-Q, L2_),
    L2-[] = L2_.
foo([], L2, L2).
foo([X|Rest], Acc, L2) :-
    fenqueue(X, Acc, Acc1),
    foo(Rest, Acc1, L2).

fenqueue(X, Qh-[X|Qt], Qh-Qt).

bar(L1, L2) :-
    bar(L1, Q-Q, L2_),
    L2-[] = L2_.
bar([], L2, L2).
bar([X|Rest], Acc, L2) :-
    push(X, Acc, Acc1),
    bar(Rest, Acc1, L2).

push(X, Qh-[X|Qt], Qt-Qh).
% Can you copy reverse and the difflist stuff above to make it so you have
% a running stack?? Otherwise IDK how to solve this problem efficiently.
%
% reverse(Xs, Ys) :-
%     reverse(Xs, [], Ys, Ys).
% 
% reverse([], Ys, Ys, []).
% reverse([X|Xs], Rs, Ys, [_|Bound]) :-
%     reverse(Xs, [X|Rs], Ys, Bound).
baz([], [], []).
baz([], Opens, Closeds).
baz([O|Chunks], Opens, Closeds) :-
    open(O),
    baz(Chunks, [O|Opens], Closeds).
baz([C|Chunks], Opens, Closeds) :-
    closed(C),
    Opens = [O|Rest],
    open_close(O, C),
    baz(Chunks, Rest, Closeds).
baz([C|Chunks], Opens, Closeds) :-
    closed(C),
    Opens = [O|Rest],
    mismatched_open_close(O, C),
    Pred =.. [first_illegal, C],
    assertz(Pred), fail.

solve2([], 0).
solve2([Chunk|Chunks], ErrorScore) :-
    (
        baz(Chunk, _, _) ->
        solve2(Chunks, ErrorScore) ;
        (
            first_illegal(Bracket) ->
            (
                points(Bracket, Points),
                ErrorScore #= Points + ErrorScore0,
                retractall(first_illegal(_)),
                solve2(Chunks, ErrorScore0)
            ) ;
            solve2(Chunks, ErrorScore)
        )
    ).

solution_10_01(ErrorScore) :-
    slurp(Lines, '10/input.txt'),
    maplist(string_chars, Lines, Chunks),
    solve2(Chunks, ErrorScore),
    retractall(first_illegal(_)).
