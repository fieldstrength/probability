module Probability.Examples.RandomWalk

import Probability.Core
import Probability.Monad


%default total


step : Trans Integer
step n = flat [n-1,n+1]

steps : Nat -> Trans Integer
steps n = perform n step

walk : Nat -> Prob Integer
walk n = pure 0 >>= steps n
