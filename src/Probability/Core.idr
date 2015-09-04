module Probability.Core

import Probability.Utils


%default total


||| Representation of a probability distribution
abstract
data Probability p a = Pr (List (a,p))


%access public


---- Types ----

Prob : Type -> Type
Prob t = Probability Float t

Transition : Type -> Type -> Type
Transition a b = a -> Prob b

Trans : Type -> Type
Trans t = Transition t t

---- Projections ----

runProb : Probability p a -> List (a,p)
runProb (Pr l) = l

objects : Probability p a -> List a
objects (Pr l) = fst <$> l

probs : Probability p a -> List p
probs (Pr l) = snd <$> l


---- Distributions ----

certainly : Num p => a -> Probability p a
certainly x = Pr [(x,1)]

flat : List a -> Prob a
flat l = let s = (1 / (cast $ length l))
  in Pr $ (\x => (x,s)) <$> l

shape : List a -> List Float -> Prob a
shape xs ps = Pr $ zipWith MkPair xs (normalize ps)


---- Instances ----

instance Functor (Probability p) where
  map f (Pr l) = Pr $ left f <$> l

instance Num p => Applicative (Probability p) where
  pure     = certainly
  fm <*> m = Pr [ (f x, q*w) | (f,w) <- runProb fm, (x,q) <- runProb m ]

instance Num p => Monad (Probability p) where
  d >>= f = Pr [ (y, q*w) | (x,w) <- runProb d, (y,q) <- runProb (f x) ]


---- Consolidation ----

gatherer : (Eq a, Eq p, Num p) => List (a,p) -> List (a,p)
gatherer [] = []
gatherer ((x,p) :: xs) = assert_total $  -- why is assert_total needed?
   let lyln = splitBy (\(z,_) => z == x) xs
       newp = (+) p . sum $ map snd (fst lyln)
   in  (x,newp) :: gatherer (snd lyln)

||| Combine any identical elements of a distribution.
||| Ideally we'd want this to be included in the definition of
||| the monadic bind, but we can't due to the 'Eq a' constraint.
gather : (Eq a, Eq p, Num p) => Probability p a -> Probability p a
gather (Pr l) = Pr $ gatherer l


---- Other Functions ----

||| Remove some element from a list
except : Eq a => List a -> a -> List a
except []      _ = []
except (x::xs) y =
  case (x==y) of
       True  => except xs y
       False => x :: except xs y

||| Remove from some list the elements of another list
removing : Eq a => List a -> List a -> List a
removing l []      = l
removing l (x::xs) = (l `except` x) `removing` xs
