module Probability.Monad

import Probability.Core


%default total


infixr 6 >=>
(>=>) : Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = (>>= g) . f

infixl 6 <=<
(<=<) : Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) = flip (>=>)


sequ : Monad m => List (a -> m a) -> a -> m a
sequ = foldl (>=>) return


perform : Monad m => Nat -> (a -> m a) -> a -> m a
perform n g = sequ $ replicate n g
