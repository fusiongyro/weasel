module Classic where

import Meta

import Control.Monad
import Control.Monad.Trans
import Data.Function
import Data.List
import System.Exit
import System.Random

-- Step #2 is the reproduce function:

reproduce :: DNA -> Weasel [DNA]
reproduce s = do
  pop <- totalPopulation
  return $ replicate pop s

-- Step #3 is the mutate function:

mutate :: [DNA] -> Weasel [DNA]
mutate = mapM mutateDNA
 
mutateDNA :: DNA -> Weasel DNA
mutateDNA = mapM mutate'
  where
    mutate' :: Char -> Weasel Char
    mutate' char = do
      rate <- mutationRate
      randomValue <- lift randomIO :: Weasel Double
      if randomValue <= rate then lift randomChar else return char


-- Here we deviate from the basic plan. We sort the strings by score. If
-- the score is 28, we finish. Otherwise, we take the highest scoring one
-- and use it to recur on the reproduction step.

classicSimulation :: Int -> DNA -> Weasel ()
classicSimulation n currentBest = do
  target1 <- target
  fitness1 <- fitness currentBest
  liftIO $ putStrLn $ "Generation " ++ show n ++ ": " ++ currentBest ++ " (distance = " ++ show fitness1 ++ ")"
  when (currentBest == target1) (liftIO exitSuccess)
  population <- reproduce currentBest
  generation <- mutate population
  winner <- best generation
  classicSimulation (n+1) winner

best :: [DNA] -> Weasel DNA
best population = do
  pop <- forM population $ \dna -> do
    f <- fitness dna
    return (dna, f)
  return $ fst $ last $ sortBy (compare `on` snd) pop

fitness :: DNA -> Weasel Int
fitness test = do
  t <- target
  return $ sum $ map (\x -> if x == True then 1 else 0) $ zipWith (==) test t

simulate :: Int -> DNA -> Weasel ()
simulate n currentBest = do
  m <- mode
  simulate' m n currentBest
  where
    simulate' ClassicMode n currentBest = classicSimulation n currentBest
