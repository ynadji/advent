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

