module Sexual where

import Meta

import Control.Monad
import Control.Monad.Trans
import Data.Function
import Data.List
import System.Exit
import System.Random

reproduceWith :: DNA -> DNA -> Weasel DNA
male `reproduceWith` female = zipWithM chooseEither male female
  where
    chooseEither m f = do
      cond <- lift $ randomRIO (0,20) :: Weasel Int
      case cond of
        0 -> lift randomChar 
        n -> return $ if n `mod` 2 == 0 then f else m

-- Fitness is calculated by using the edit distance.

fitness :: DNA -> Weasel Int
fitness test = do
  target' <- target
  return $ fitnessTo target' test

fitnessTo :: DNA -> DNA -> Int
fitnessTo target test = 28 - (sum $ map (\x -> if x == True then 1 else 0) $ zipWith (==) test target)

-- Now we can create a random population of size N.

generatePopulation :: Int -> Weasel [DNA]
generatePopulation n = sequence $ replicate n randomDNA

-- Once we have a population we can select the top N most fit.

nMostFit :: Int -> [DNA] -> Weasel [DNA]
nMostFit n population = do
  target' <- target
  return $ take n $ sortBy (compare `on` (fitnessTo target')) population

-- Let's select the most-fit member too, just for fun during the output.

mostFit :: [DNA] -> Weasel DNA
mostFit population = do
  mostFit <- nMostFit 1 population
  return $ mostFit !! 0

nextGeneration :: [DNA] -> Weasel [DNA]
nextGeneration (x:y:xs) = do
  new  <- x `reproduceWith` y
  rest <- nextGeneration (y:xs)
  return (new:rest)
nextGeneration _ = return []

-- Now we can bundle it up and simulate a generation.

simulateGeneration :: Int -> [DNA] -> Weasel [DNA]
simulateGeneration decreaseTo population = do
  mostFit <- nMostFit decreaseTo population
  half1 <- nextGeneration mostFit
  half2 <- nextGeneration mostFit
  return $ half1 ++ half2

-- Now let's make the whole simulation and tie it together.

simulation :: Int -> [DNA] -> Weasel ()
simulation n population = do
  target' <- target
  currentBest <- mostFit population
  lift $ putStrLn $ "Generation " ++ show n ++ ": " ++ currentBest ++ " (distance = " ++ show (fitnessTo target' currentBest) ++ ")"
  when (currentBest == target') (liftIO exitSuccess)
  nextGen <- simulateGeneration 20 population
  simulation (n+1) nextGen

-- Let's make the main:

main = do
  initialPopulation <- generatePopulation 100
  simulation 1 initialPopulation
