['../prolog/utils'].

breed(Fish, NumFish, MaxTicks) :- once(breed_(Fish, NumFish, MaxTicks, 0)).

tick_day([], []).
tick_day([0|Rest], [6,8|NextRest]) :-
    tick_day(Rest, NextRest).
tick_day([Fish|Rest], [NextFish|NextRest]) :-
    NextFish #= Fish - 1,
    tick_day(Rest, NextRest).

breed_(Fish, NumFish, HitMax, HitMax) :-
    length(Fish, NumFish).
breed_(Fish, NumFish, MaxTicks, CurrTick) :-
    once(tick_day(Fish, NextFish)),
    NextTick #= CurrTick + 1,
    breed_(NextFish, NumFish, MaxTicks, NextTick).

solution_06_01(NumFish) :-
    slurp(Line_, '06/input.txt'),
    nth1(1, Line_, Line),
    split_string(Line, ',', '', NumChars),
    maplist(atom_number, NumChars, Fish),
    breed(Fish, NumFish, 80).

tick_day_fast([B0, B1, B2, B3, B4, B5, B6, B7, B8],
              [N0, N1, N2, N3, N4, N5, N6, N7, N8]) :-
    N0 #= B1,
    N1 #= B2,
    N2 #= B3,
    N3 #= B4,
    N4 #= B5,
    N5 #= B6,
    N7 #= B8,
    N6 #= B7 + B0,
    N8 #= B0.

fast_breed_(FishBuckets, NumFish, HitMax, HitMax) :-
    sum_list(FishBuckets, NumFish).
fast_breed_(FishBuckets, NumFish, MaxTicks, CurrTick) :-
    tick_day_fast(FishBuckets, NewFishBuckets),
    NextTick #= CurrTick + 1,
    fast_breed_(NewFishBuckets, NumFish, MaxTicks, NextTick).

fast_breed(Fish, NumFish, MaxTicks) :-
    fish_buckets(Fish, FishBuckets),
    fast_breed_(FishBuckets, NumFish, MaxTicks, 0).

fish_buckets(Fishies, [B0, B1, B2, B3, B4, B5, B6, B7, B8]) :-
    msort(Fishies, Sorted),
    clumped(Sorted, Counts),
    (member(0-B0, Counts) ; B0 #= 0),
    (member(1-B1, Counts) ; B1 #= 0),
    (member(2-B2, Counts) ; B2 #= 0),
    (member(3-B3, Counts) ; B3 #= 0),
    (member(4-B4, Counts) ; B4 #= 0),
    (member(5-B5, Counts) ; B5 #= 0),
    (member(6-B6, Counts) ; B6 #= 0),
    (member(7-B7, Counts) ; B7 #= 0),
    (member(8-B8, Counts) ; B8 #= 0).

solution_06_02(NumFish) :-
    slurp(Line_, '06/input.txt'),
    nth1(1, Line_, Line),
    split_string(Line, ',', '', NumChars),
    maplist(atom_number, NumChars, Fish),
    fast_breed(Fish, NumFish, 256).

fast_and_clean(NumFish, Buckets, 0) :- sum_list(Buckets, NumFish).
fast_and_clean(NumFish, [B0, B1, B2, B3, B4, B5, B6, B7, B8], N) :-
    N_ #= N - 1,
    N6 #= B7 + B0,
    fast_and_clean(NumFish, [B1, B2, B3, B4, B5, B6, N6, B8, B0], N_).

solution_06_02_new(NumFish) :-
    slurp(Line_, '06/input.txt'),
    nth1(1, Line_, Line),
    split_string(Line, ',', '', NumChars),
    maplist(atom_number, NumChars, Fish),
    fish_buckets(Fish, Buckets),
    fast_and_clean(NumFish, Buckets, 256).
