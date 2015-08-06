module Probability.Monad

import Probability.Core

%default total


infixl 2 >=>
(>=>) : Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = (>>= g) . f


sequ : Monad m => List (a -> m a) -> a -> m a
sequ = foldl (>=>) return


perform : Monad m => Nat -> (a -> m a) -> a -> m a
perform n g = sequ $ replicate n g
