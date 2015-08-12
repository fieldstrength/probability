# Probability
_Probabilistic computation in Idris._

The __probability__ package provides facilities for manipulation and visualization of probability distributions.

It is heavily inspired by the [Probabilistic Functional Programming](https://web.engr.oregonstate.edu/~erwig/pfp/) library for Haskell (a more recent evolution of which is on [hackage](https://hackage.haskell.org/package/probability)) and the associated [Functional Perl](https://web.engr.oregonstate.edu/~erwig/papers/abstracts.html#JFP06a) by Martin Erwig and Steve Kollmansberger.

Much of the expressivity and power of this approach comes from the natural functorial, applicative and monadic structures on probability distributions. The implementation of these structures via the corresponding type classes is the core of the library, and is borrowed almost exactly from PFP.

The package needs idris 0.9.19 or the current Github version.

### Examples
#### Six-Sided Die

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

To do something more interesting, we'd like to work not just with distributions over _objects_, but over _processes_, i.e. functions. So for example we can turn our die into a distribution over the _adding functions_ `(+1)` to `(+6)`.

```
:let rollD = map (+) die
```
Let's see what results from applying our probabilistic function `rollD` to our standard distribution over integers `die`. In other words, what is the outcome distribution on the roll of two dice? This is carried out by the `Applicative` instance and its operator `<*> : Prob (a -> b) -> Prob a -> Prob b`.

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
*Probability> :let rollDie = \n => map (+n) die
*Probability> die >>= rollDie >>= rollDie
```

This is defined in the `Examples.Dice` module, but we want to emphasize that it's possible to do all of this from the REPL.

Clearly we'll want to abstract this pattern, and this is easily accomplished with some monadic machinery defined in the `Monad` module. For example we can bind together an arbitrary number of monadic operations using the `perform` function, allowing us to define:

```idris
rollDice : Nat -> Trans Integer
rollDice n = perform n rollDie
```

Finally, since we'll usually want to end up with a distribution over integers again, starting from zero, we define:

```idris
roll : Nat -> Prob Integer
roll n = pure 0 >>= rollDice n
```

Many variations on these basic approaches are possible.

```
*Probability> :x display $ roll 4
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

The [Monty Hall problem](https://en.wikipedia.org/wiki/Monty_Hall_problem) is famous brain teaser which exposes the fact that our intuitions about probability can go badly astray.

A contestant on a game show is presented with three doors, one of which has a prize behind it. They make an initial guess which door holds the prize. Next the host opens one of the other two doors, revealing that there is no prize behind it. Finally, the contestant can choose either to stay with their original choice or switch to the other unopened door. So what is the best strategy for the contestant?

Many people erroneously believe that switching makes no difference. However the correct answer is that it's always better to switch, increasing the chance of success from 1/3 to 2/3. So let's use our package to analyze the problem.

Start with a simple data type to represent the three doors.

```idris
data Door = One | Two | Three

doors : List Door
doors = [One,Two,Three]
```

Our graphing and other functionality of the package requires `Eq` and `Show` instances which are defined in the `Examples.MontyHall` module, but will not be shown here.

We'll represent the state of the game by a `Vect` of `Door`s, encoding:

  1. The door containing the prize
  2. The contestant's first choice
  3. The door opened by the host
  4. The contestant's final choice

We'll define a series of transition functions that step the game through its 4 stages. First a couple of type synonym helper functions.

```idris
Monty : Nat -> Type
Monty n = Vect n Door

Step : Nat -> Type
Step n = Transition (Monty n) (Monty (S n))
```

So we'll be using dependent types to enforce an important property of our functions, but this does not mean we need to write them out in detail repeatedly.

The beginning of the game is modeled as a flat distribution over doors representing the prize location. They're placed in a vector to match our game state type defined above.

```idris
placePrize : Prob (Monty 1)
placePrize = map (:: Nil) $ flat doors
```

Now our first transition function. We're using a specialized operator `::~` to append elements to the _end_ of a vector, so that game histories can be read off more easily. As a reminder, the type `Step 1` Means `Transition (Monty 1) (Monty 2)` which in turn means `Vect 1 Door -> Prob (Vect 2 Door)`.

```idris
firstChoice : Step 1
firstChoice v = map (v ::~) $ flat doors
```

So far, the game state we can build up is `placePrize >>= firstChoice`. At this point the distribution remains completely flat.

The first non-trivial game logic comes in at this next step, which is also where our intuition can get thrown off. The host will only choose a door to open if it is neither the prize door nor the contestant's chosen door.

```idris
openOne : Step 2
openOne v@[p,c] = map (v ::~) $ flat $ doors `removing` [p,c]
```

Now it may be instructive to consult the graph.

```
*Probability> :x display $ placePrize >>= firstChoice >>= openOne
[Door #1, Door #1, Door #2]| 05.55%  ███████████████
[Door #1, Door #1, Door #3]| 05.55%  ███████████████
[Door #1, Door #2, Door #3]| 11.11%  ██████████████████████████████
[Door #1, Door #3, Door #2]| 11.11%  ██████████████████████████████
[Door #2, Door #1, Door #3]| 11.11%  ██████████████████████████████
[Door #2, Door #2, Door #1]| 05.55%  ███████████████
[Door #2, Door #2, Door #3]| 05.55%  ███████████████
[Door #2, Door #3, Door #1]| 11.11%  ██████████████████████████████
[Door #3, Door #1, Door #2]| 11.11%  ██████████████████████████████
[Door #3, Door #2, Door #1]| 11.11%  ██████████████████████████████
[Door #3, Door #3, Door #1]| 05.55%  ███████████████
[Door #3, Door #3, Door #2]| 05.55%  ███████████████
```

The non-flat distribution results from the fact that, when the contestant has successfully chosen the prize door, the host can choose from two different doors to open, whereas otherwise he is forced to open a particular door. So the short bars with 5.55% probability correspond to the winning game histories, adding up to a total chance of 1/3. However if the player can utilize their knowledge of this distribution they can up their odds to 2/3.

The switch and stay strategies can be described as follows.

```idris
stay : Step 3
stay v@[p,c,o] = certainly $ v ::~ c

switch : Step 3
switch v@[p,c,o] = map (v ::~) $ flat $ doors `removing` [c,o]
```

Putting everything together, the two distributions corresponding to the two game strategies are:

```idris
stayGame : Prob (Monty 4)
stayGame = placePrize >>= firstChoice >>= openOne >>= stay

switchGame : Prob (Monty 4)
switchGame = placePrize >>= firstChoice >>= openOne >>= switch
```

By the way, we've defined a couple of additional types that wrap our game vectors to assist in reading our graphs. `GameScore` introduces a modified `Show` instance to annotate the win/loss status (and uses the first door choice if the final has yet to be made), while `GameOutcome` also modifies the `Eq` instance so that we can collapse all the histories with the same outcome. In other words, it performs the sums to verify that the overall outcome probabilities are what they are.


```
*Probability> :x display $ Score <$> switchGame
 LOSE [Door #1, Door #1, Door #2, Door #3]| 05.55%  ███████████████
 LOSE [Door #1, Door #1, Door #3, Door #2]| 05.55%  ███████████████
 WIN  [Door #1, Door #2, Door #3, Door #1]| 11.11%  ██████████████████████████████
 WIN  [Door #1, Door #3, Door #2, Door #1]| 11.11%  ██████████████████████████████
 WIN  [Door #2, Door #1, Door #3, Door #2]| 11.11%  ██████████████████████████████
 LOSE [Door #2, Door #2, Door #1, Door #3]| 05.55%  ███████████████
 LOSE [Door #2, Door #2, Door #3, Door #1]| 05.55%  ███████████████
 WIN  [Door #2, Door #3, Door #1, Door #2]| 11.11%  ██████████████████████████████
 WIN  [Door #3, Door #1, Door #2, Door #3]| 11.11%  ██████████████████████████████
 WIN  [Door #3, Door #2, Door #1, Door #3]| 11.11%  ██████████████████████████████
 LOSE [Door #3, Door #3, Door #1, Door #2]| 05.55%  ███████████████
 LOSE [Door #3, Door #3, Door #2, Door #1]| 05.55%  ███████████████
```

```
*Probability> :x display $ Outcome <$> switchGame
 LOSE | 33.33%  ███████████████
 WIN  | 66.66%  ██████████████████████████████
```

These hopefully give a sense of some of the general techniques that can be used.
