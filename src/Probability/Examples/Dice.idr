module Probability.Examples.Dice

import Probability.Core
import Probability.Monad


%default total


die : Prob Integer
die = flat [1..6]

rollDie : Trans Integer
rollDie n = (+ n) <$> die

rollDice : Nat -> Trans Integer
rollDice n = perform n rollDie

roll : Nat -> Prob Integer
roll n = pure 0 >>= rollDice n
