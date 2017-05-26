module Probability.Examples.MontyHall

import Probability.Core
import Probability.Utils

import Data.Vect


%default total


data Door = One | Two | Three

Eq Door where
  One   == One   = True
  Two   == Two   = True
  Three == Three = True
  _     == _     = False

Show Door where
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

||| Check if the contestant is currently winning.
||| Compare the prize door to either their final choice,
||| or their initial choice if the final choice isn't made yet.
score : Monty (S (S k)) -> Bool
score {k} v = case k of
  S (S n) => index 3 v == index 0 v
  _       => index 1 v == index 0 v


data GameScore = Score (Vect (S (S n)) Door)

Eq GameScore where
  (Score v) == (Score w) = vecEq v w

Show GameScore where
  show (Score v) = let status = if score v then " WIN  " else " LOSE "
    in  status ++ show v



data GameOutcome = Outcome (Monty (S (S n)))

Eq GameOutcome where
  (Outcome v) == (Outcome w) = score v == score w

Show GameOutcome where
  show (Outcome v) = if score v then " WIN  " else " LOSE "


{- We'll be building up a Vect 4 Door encoding:

  [PrizeLocation, FirstChoice, Opened, FinalChoice]  -}


||| Type of transition probabilities between game states
Step : Nat -> Type
Step n = Transition (Monty n) (Monty (S n))


placePrize : Prob (Monty 1)
placePrize = map (:: Nil) $ flat doors

firstChoice : Step 1
firstChoice v = map (v ::~) $ flat doors

chooseOne : Step 1
chooseOne v = map (v ::~) $ certainly One

openOne : Step 2
openOne v@[p,c] = map (v ::~) $ flat $ doors `removing` [p,c]

stay : Step 3
stay v@[p,c,o] = certainly $ v ::~ c

switch : Step 3
switch v@[p,c,o] = map (v ::~) $ flat $ doors `removing` [c,o]


stayGame : Prob (Monty 4)
stayGame = placePrize >>= firstChoice >>= openOne >>= stay

switchGame : Prob (Monty 4)
switchGame = placePrize >>= firstChoice >>= openOne >>= switch


stayOutcome : Prob GameOutcome
stayOutcome = Outcome <$> stayGame

stayHistory : Prob GameScore
stayHistory = Score <$> stayGame


switchOutcome : Prob GameOutcome
switchOutcome = Outcome <$> switchGame

switchHistory : Prob GameScore
switchHistory = Score <$> switchGame
