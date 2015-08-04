module Probability.Examples.Dice

import Probability


%default total


die : ProbF Integer
die = flat [1..6]

addRoll : Transition Integer  -- Integer -> ProbF Integer
addRoll n = (+ n) <$> die

roll2 : ProbF Integer
roll2 = gather $ die >>= addRoll

roll3 : ProbF Integer
roll3 = gather $ die >>= addRoll >>= addRoll

