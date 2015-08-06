# Probability
_Probabilistic computation in Idris._

The __probability__ package provides facilities for manipulation and visualization of probability distributions.

It is heavily inspired by the [Probabilistic Functional Programming](https://web.engr.oregonstate.edu/~erwig/pfp/) library for Haskell (a more recent evolution of which is on [hackage](https://hackage.haskell.org/package/probability)) and the associated [Functional Perl](https://web.engr.oregonstate.edu/~erwig/papers/abstracts.html#JFP06a) by Martin Erwig and Steve Kollmansberger.

Much of the expressivity and power of this approach comes from the natural functorial, applicative and monadic structures on probability distributions. The implementation of these structures via the corresponding type classes is the core of the library, and is borrowed almost exactly from PFP.

### Examples
##### Six-Sided Die

For a simple example, we can model a standard die as a flat distribution over integers.

```
*Probability> :let die = flat [1..6]
*Probability> :x display die
1| 16.66%  ██████████████████████████████
2| 16.66%  ██████████████████████████████
3| 16.66%  ██████████████████████████████
4| 16.66%  ██████████████████████████████
5| 16.66%  ██████████████████████████████
6| 16.66%  ██████████████████████████████
```

To do something more interesting, we'd like to work not just with distributions over _objects_, but over _processes_, i.e. functions. We could turn our die into a distribution over the _adding functions_ `(+1)` to `(+6)`.

```
:let rollD = map (+) die
```
Let's see what results from applying our probabilistic function `rollD` to our standard distribution over integers `die`. This is carried out by the `Applicative` instance and its operator `<*> : Prob (a -> b) -> Prob a -> Prob b`.

```
*Probability> :x display $ rollD <*> die
 2| 02.77%  ████▉
 3| 05.55%  █████████▉
 4| 08.33%  ██████████████▉
 5| 11.11%  ███████████████████▉
 6| 13.88%  ████████████████████████▉
 7| 16.66%  ██████████████████████████████
 8| 13.88%  ████████████████████████▉
 9| 11.11%  ███████████████████▉
10| 08.33%  ██████████████▉
11| 05.55%  █████████▉
12| 02.77%  ████▉
```

Note that our original distribution `die` is the same thing as `rollD <*> certainly 0`.

In many circumstances `Applicative` won't be general enough, and we'll need the Monad instance, i.e. an operator that depends on `a -> Prob b` instead of `Prob (a -> b)`. As a simple example we could do something like

```
*Probability> :let rollAgain = \n => map (+n) die
*Probability> die >>= rollAgain >>= rollAgain
```

The `Examples.Dice` module uses some monadic machinery to utilize this. Many variations on the basic idea are possible.

```
*Probability> :x display $ pure 0 >>= roll 4
 4| 00.07%  ▏
 5| 00.30%  ▊
 6| 00.77%  ██
 7| 01.54%  ████
 8| 02.70%  ███████▏
 9| 04.32%  ███████████▍
10| 06.17%  ████████████████▍
11| 08.02%  █████████████████████▎
12| 09.64%  █████████████████████████▌
13| 10.80%  ████████████████████████████▋
14| 11.26%  ██████████████████████████████
15| 10.80%  ████████████████████████████▋
16| 09.64%  █████████████████████████▌
17| 08.02%  █████████████████████▎
18| 06.17%  ████████████████▍
19| 04.32%  ███████████▍
20| 02.70%  ███████▏
21| 01.54%  ████
22| 00.77%  ██
23| 00.30%  ▊
24| 00.07%  ▏
```
#### The Monty Hall Problem

To be continued...
