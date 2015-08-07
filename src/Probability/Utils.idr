module Probability.Utils

import Data.Vect


%default total


infixl 5 ::~

||| Append at the end of a vector
(::~) : Vect n a -> a -> Vect (S n) a
(::~) {n} v x = rewrite plusCommutative 1 n in v ++ [x]

||| Equality on vectors not necessarily of the same length
vecEq : Eq a => Vect n a -> Vect m a -> Bool
vecEq (x::xs) (y::ys) = x == y && vecEq xs ys
vecEq []      []      = True
vecEq _       _       = False


normalize : List Float -> List Float
normalize l = (/ (sum l)) <$> l

zipWith' : (a -> b -> c) -> List a -> List b -> List c
zipWith' f (x::xs) (y::ys) = f x y :: zipWith' f xs ys
zipWith' f _       []      = []
zipWith' f []      _       = []

left : (a -> c) -> (a,b) -> (c,b)
left f (x,y) = (f x, y)
