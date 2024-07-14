{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
    deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
    random = first DV . randomR (1, 6)
    randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}
    deriving (Show)

-- exercise 2
maxArmyNum :: Battlefield -> (Army, Army)
maxArmyNum (Battlefield atk dfd) = (maxAtk, maxDfd)
  where
    maxAtk = if atk > 3 then 3 else atk - 1
    maxDfd = min dfd 2

dice :: Int -> Rand StdGen [DieValue]
dice n = sequence (replicate n die)

simulateBattle :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
simulateBattle bf atkRoll dfdRoll =
    let result = zipWith (>) atkRoll dfdRoll
     in foldl'
            ( \(Battlefield atk dfd) r ->
                if r
                    then (Battlefield atk (dfd - 1))
                    else
                        (Battlefield (atk - 1) dfd)
            )
            bf
            result

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
    atkRoll' <- atkRoll
    dfdRoll' <- dfdRoll
    return (simulateBattle bf atkRoll' dfdRoll')
  where
    (maxAtk, maxDfd) = maxArmyNum bf
    atkRoll = sort <$> dice maxAtk
    dfdRoll = sort <$> dice maxDfd

-- exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield atk dfd)
    | atk < 2 || dfd <= 0 = return bf
    | otherwise = battle bf >>= invade

-- exercise 4
prob :: [Battlefield] -> Double
prob xs =
    let (win, total) =
            foldl'
                ( \(w, s) bf ->
                    let s' = s + 1
                     in if defenders bf <= 0 then (w + 1, s') else (w, s')
                )
                (0, 0)
                xs
     in fromInteger win / fromInteger total

successProb :: Battlefield -> Rand StdGen Double
successProb bf =
    prob <$> sequence simRes
  where
    simRes = take 1000 $ repeat $ invade bf

-- evalRandIO
