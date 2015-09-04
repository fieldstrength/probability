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

left : (a -> c) -> (a,b) -> (c,b)
left f (x,y) = (f x, y)

||| Split a list into two lists according to whether a
||| predicate function returns true
splitBy : (a -> Bool) -> List a -> (List a, List a)
splitBy f l = splitter f (reverse l) ([],[])
   where splitter : (a -> Bool) -> List a -> (List a, List a) -> (List a, List a)
         splitter f []      (xs,ys) = (xs,ys)
         splitter f (z::zs) (xs,ys) = if f z then splitter f zs (z :: xs, ys)
                                             else splitter f zs (xs, z :: ys)
