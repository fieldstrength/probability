module Probability.Examples.MontyHall

import Probability.Core
import Probability.Display

import Data.Vect


%default total


data Door = One | Two | Three

instance Eq Door where
  One   == One   = True
  Two   == Two   = True
  Three == Three = True
  _     == _     = False

instance Show Door where
  show One   = "Door #1"
  show Two   = "Door #2"
  show Three = "Door #3"

doors : List Door
doors = [One,Two,Three]


||| There are four steps in the Monty Hall game.
||| We represent the n'th step by a vector of n Doors
||| corresponding to
|||    1) The door where the prize is hidden
|||    2) The first door the contestant chooses
|||    3) The door opened by the host
|||    4) The contestant's final choice
Monty : (n : Nat) -> Type
Monty n = Vect n Door

infixl 5 ::~

||| Append at the end of a vector
(::~) : Vect n a -> a -> Vect (S n) a
(::~) {n} v x = rewrite plusCommutative 1 n in v ++ [x]


win : Monty 4 -> Bool
win [p,c,o,f] = f == p

data GameHistory = History (Monty 4)

instance Eq GameHistory where
  (History v) == (History w) = v == w

instance Show GameHistory where
  show (History v) = let status = if win v then " WIN  " else " LOSE "
    in  status ++ show v


data Outcome = Out (Monty 4)

instance Eq Outcome where
  (Out v) == (Out w) = win v == win w

instance Show Outcome where
  show (Out v) = if win v then " WIN  " else " LOSE "


{- We'll be building up a Vect 4 Door encoding:

  [PrizeLocation, FirstChoice, Opened, FinalChoice]  -}


||| Type of transition probabilities between game states
Step : Nat -> Type
Step n = Transition (Monty n) (Monty (S n))

||| Remove some element from a list
except : Eq a => List a -> a -> List a
except []      _ = []
except (x::xs) y =
  case (x==y) of
       True  => except xs y
       False => x :: except xs y

||| Remove elements of some list from another list
removing : Eq a => List a -> List a -> List a
removing l []      = l
removing l (x::xs) = (l `except` x) `removing` xs


placePrize : Prob (Monty 1)
placePrize = map (:: Nil) $ flat doors

firstChoice : Step 1
firstChoice v = map (v ::~) $ flat doors

openOne : Step 2
openOne v@[p,c] = map (v ::~) $ flat $ doors `removing` [c,p]

stay : Step 3
stay v@[p,c,o] = certainly $ v ::~ c

switch : Step 3
switch v@[p,c,o] = map (v ::~) $ flat $ doors `removing` [c,o]


stayGame : Prob (Monty 4)
stayGame = placePrize >>= firstChoice >>= openOne >>= stay

switchGame : Prob (Monty 4)
switchGame = placePrize >>= firstChoice >>= openOne >>= switch


stayOutcome : Prob Outcome
stayOutcome = Out <$> stayGame

stayHistory : Prob GameHistory
stayHistory = History <$> stayGame


switchOutcome : Prob Outcome
switchOutcome = Out <$> switchGame

switchHistory : Prob GameHistory
switchHistory = History <$> switchGame
