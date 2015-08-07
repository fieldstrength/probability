module Probability.Display

import Probability.Core
import Probability.Utils


%default total


width : Float
width = 30

---- Floating Point Utils ----

castFN : Float -> Nat
castFN = cast . cast {to=Integer}

intPart : Float -> Float
intPart = cast . cast {to=Integer}

fracPart : Float -> Float
fracPart x = x - intPart x


||| Raise a float to an arbitrary integral power
fpow : Float -> Integer -> Float
fpow f p = if p >= 0 then pow f (cast p)
                     else 1 / (pow f $ cast $ abs p)


%default partial


---- Displaying Percentages ----

digit : Integer -> Char
digit 0 = '0'
digit 1 = '1'
digit 2 = '2'
digit 3 = '3'
digit 4 = '4'
digit 5 = '5'
digit 6 = '6'
digit 7 = '7'
digit 8 = '8'
digit 9 = '9'

digAt : Float -> Integer -> Integer
digAt x i = flip mod 10 $ cast $ abs $ x / (fpow 10 i)

charAt : Float -> Integer -> Char
charAt x i = digit $ digAt x i


scanUp : Float -> Integer -> Integer
scanUp x n = if x < (fpow 10 $ n + 1) then n else scanUp x (n + 1)

scanDown : Float -> Integer -> Integer
scanDown x n = if x > (fpow 10 $ -n) then -n else scanDown x (n + 1)

||| Find the leading digit of a Float
maxdig : Float -> Integer
maxdig x = if x > 1 then scanUp x 0
                    else scanDown x 1

||| Show 4 digits (not necessarily significant) of a percentage
showPercent : Float -> String
showPercent x = let y  = 100 * x
                    s1 = pack $ (charAt y) <$> [1,0]
                    s2 = pack $ (charAt y) <$> [-1,-2]
  in " " ++ s1 ++ "." ++ s2 ++ "%  "


%default total


---- Display Bars ----

tipChars : List String
tipChars = ["▉",
            "▊",
            "▋",
            "▌",
            "▍",
            "▎",
            "▏"]

tipVals : List Float
tipVals = (+ 0.0625) . (/8) . cast <$> reverse [1..7]
     -- = [15/16, 13/16, 11/16, 9/16, 7/16, 5/16, 3/16]


tips : List (Float,String)
tips = zipWith' MkPair tipVals tipChars

selectTip : Float -> String
selectTip x = let l = filter (\p => fst p < x) tips
  in case l of
     [] => ""
     ((f,s)::ss) => s


bar : Float -> String
bar f = pack (replicate (castFN f) '█') ++ selectTip (fracPart f)

bars : List Float -> List String
bars l = let mx = foldr max 0 l
  in map bar $ map (* width/mx) l


labels : List String -> List String
labels l = let mx = foldr max 0 (length <$> l)
  in (\s => (pack $ replicate (mx - length s) ' ') ++ s ++ "|") <$> l


%default partial


disp : (Show a, Eq a) => Prob a -> List String
disp p = let q  = gather p
             ls = labels . map show $ objects q
             bs = bars $ probs q
             ps = showPercent <$> probs q
             ss = zipWith' (++) ls ps
  in zipWith' (++) ss bs

display : (Show a, Eq a) => Prob a -> IO ()
display p = putStrLn . unlines $ disp p
