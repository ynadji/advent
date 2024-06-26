# Advent of Code

I've optimistically included folders by year. We'll see if I do this again next
year or not.

## 2021

I've always wanted to improve my Prolog, so let's try to use it for AoC!
When a solution led me to learn something interesting or useful about Prolog,
I'll include it in the list below. Sometimes those examples will have a comment at
the beginning of the file explaining the approach and what I learned. A resource
I found myself revisiting constantly was Markus Triska's excellent [The Power of
Prolog](https://www.metalevel.at/prolog) page. If you're interested in getting
started with Prolog, I'd highly recommend reading it straight through, at least
up to and including Section 20 ("Combinatorial Optimization"). It's a very
helpful resource.

All solutions written/tested with SWI-Prolog:

```
yacin@sif /zfs/home/yacin/code/advent/2021/10 master*
¡ swipl --version
SWI-Prolog version 8.4.1 for x86_64-linux
```

### Helpful Concepts

- [`utils.pl`](./2021/utils.pl) shows how to use [DCGs](https://www.metalevel.at/prolog/dcg) to read input line by line. `slurp/2` is the one I used the most. DCG building block taken from [stackoverflow](https://stackoverflow.com/a/4805709/5586983).
- Using `assertz/1` and [counting the predicates](./2021/05/05.pl) or [storing intermediate results](./2021/10/10.pl). The latter is particularly helpful when base cases that result in failure has generated intermediate data that the caller needs.
- Using CLP(FD) for [constraint solving](./2021/08/08.pl).
- A hacky way to map a character to a variable named by [the same letter](./2021/08/08.pl).
- Some [examples](./2021/04/04.pl) of [using](./2021/10/10.pl) `profile/1`.
- I haven't used it yet in a solution, but [difference lists and queues](./2021/queue.pl) will likely be needed at some point.

## 2023

I started this in `awk/bash` but quickly bailed. Working through the exercises
again but this time in Common Lisp. All solutions written/tested with SBCL:

```
¡ sbcl --version
SBCL 2.4.1
```

### Helpful Links
- https://lispcookbook.github.io/cl-cookbook/
