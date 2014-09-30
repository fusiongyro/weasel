module Util where

import Control.Monad (zipWithM)
import Data.Char (ord,chr)
import Data.Function (on)
import Data.List (sortBy, tails)
import Text.Printf (printf)
import System.Random (randomRIO, randomIO)
  
type DNA = String

class EvolutionaryAlgorithm a where
  -- | Generates an initial population suitable for this algorithm.
  generatePopulation :: a -> Int -> IO [DNA]

  -- | Selects the fittest members of this population according to the algorithm.
  selectFittest      :: a -> DNA -> [DNA] -> IO [DNA]

  -- | Breeds the fittest members of the population using the algorithm.
  breed              :: a -> [DNA] -> IO [DNA]


-- | Logs a status message for the current state of affairs.
logStatus :: Int -> DNA -> [DNA] -> IO ()
logStatus n target (fittest:_) =
  printf "Generation %03d: %s (distance = %d)\n" n fittest (fitnessTo target fittest)

-- Random utilities
charFor :: Int -> Char
charFor 26 = ' '
charFor i = (chr (ord 'A' + i))

-- The randomChar function gives us a random character in the range we want.

randomChar :: IO Char
randomChar = do
  i <- randomRIO (0,26)
  return $ charFor i

-- The `randomString` function generates a completely random string,
-- containing just the 26 uppercase alphabetic characters and the space.

randomString :: Int -> IO String
randomString 0         = return ""
randomString n | n > 0 = do
  c <- randomChar
  s <- randomString (n-1)
  return $ c : s

-- Step #1 is the randomDNA function:

randomDNA :: Int -> IO DNA
randomDNA length = randomString length


-- | The common entry point to the evolutionary algorithm
weaselEvolver :: (EvolutionaryAlgorithm ea) => ea -> DNA -> IO Int
weaselEvolver ea target = do
  -- generate the initial population
  initialPopulation <- generatePopulation ea (length target)
  
  -- begin the loop at generation 1
  evolve 1 initialPopulation
  
  where
    evolve generation population = do
      -- select the fittest
      fittest <- selectFittest ea target population
      
      -- output a status message
      logStatus generation target fittest
      
      -- generate the next population by breeding the fittest
      nextPopulation <- breed ea fittest
      
      -- if we have found the target, we are done; otherwise recur
      if head fittest == target
        then return generation
        else evolve (generation + 1) nextPopulation

-- utility methods
      
-- | Mutate all of the DNA strings we have according to the supplied
--   mutation rate.
mutateAll :: Double -> [DNA] -> IO [DNA]
mutateAll mutationRate dna = mapM (mutate mutationRate) dna

-- | Mutate a given DNA string. Use the mutation rate to decide if a
-- given character will be mutated.
mutate :: Double -> DNA -> IO DNA
mutate mutationRate = mapM mutate'
  where
    mutate' :: Char -> IO Char
    mutate' char = do
      randomValue <- randomIO :: IO Double
      if randomValue <= mutationRate then randomChar else return char

-- | Return an integer score indicating how similar the two strings are.
fitnessTo :: DNA -> DNA -> Int
fitnessTo left right = sum $ zipWith score left right
  where
    score x y = if x == y
                  then 1
                  else 0

-- | Sorts the DNA by fitness.
sortByFitness :: DNA -> [DNA] -> [DNA]
                       -- Higher scores are better, which is why we
                       -- need to flip the list over after sorting
sortByFitness target = reverse . sortBy (compare `on` fitnessTo target) 

-- | Combinations. Needed for sexual reproduction. Code stolen from H99 solutions.
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys    <- combinations (n-1) xs' ]

