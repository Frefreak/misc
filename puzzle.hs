{-# LANGUAGE OverloadedStrings #-}
import Data.Ratio
import Data.List
import qualified Data.Text as T
import Data.Monoid

type Solver = (T.Text, Rational)

ratToSol :: Rational -> Solver
ratToSol r = (show' r, r)

-- pretty print rational number
show' :: Rational -> T.Text
show' r = let (a, b) = (numerator r, denominator r) in
    if b == 1 then T.pack (show a)
                    else T.pack $ show a <> "/" <> show b

possibleResults :: Solver -> Solver -> [Solver]
possibleResults (astr, a) (bstr, b) =
    let format op = "(" <> astr <> " " <> op <> " " <> bstr <> ")"
        format' op = "(" <> bstr <> " " <> op <> " " <> astr <> ")"
        atimesb = format "*"
        adivb = format "/"
        adivb' = format' "/"
        aplusb = format "+"
        aminusb = format "-"
        aminusb' = format' "-"
    in
    case b of
        0 -> [(atimesb, a * b), (aplusb, a + b), (aminusb, a - b), (aminusb', b - a)]
        _ -> [(atimesb, a * b), (aplusb, a + b),
                (aminusb, a - b), (adivb, a / b), (adivb', b / a), (aminusb', b - a)]

allPossibleResults :: [Rational] -> [Solver]
allPossibleResults [] = []
allPossibleResults rs = foldr
    (\n res -> concatMap (possibleResults $ ratToSol n) res)
    [ratToSol $ last rs] (init rs)

hasSolution :: [Rational] -> Rational -> Bool
hasSolution ls target =
    target `elem` map snd (concatMap allPossibleResults $ permutations ls)

findSolution' :: [Rational] -> Rational -> [T.Text]
findSolution' ls target =
    let allRes = concatMap allPossibleResults (permutations ls)
    in map fst $ (filter (\s -> snd s == target) allRes)

findSolution :: [Rational] -> Rational -> T.Text
findSolution ls tar =
    let solutions = findSolution' ls tar
    in if null solutions then "No solution" else head solutions

