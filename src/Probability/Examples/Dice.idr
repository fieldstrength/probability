module Probability.Examples.Dice

import Probability.Core
import Probability.Monad


%default total


die : Prob Integer
die = flat [1..6]

rollDie : Trans Integer
rollDie n = (+ n) <$> die

roll : Nat -> Trans Integer
roll n = perform n rollDie
