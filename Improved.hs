module Main where

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

-- Classic
data ClassicWeasel = ClassicWeasel { cPopulation   :: Int
                                   , cMutationRate :: Double }
                     deriving (Show, Eq)

defaultClassicWeasel = ClassicWeasel { cPopulation = 100, cMutationRate = 0.05 }

instance EvolutionaryAlgorithm ClassicWeasel where
  generatePopulation ea length = do
    -- make a random initial DNA
    initialDNA <- randomDNA length
    
    -- replicate it as many times as we want
    return $ replicate (cPopulation ea) initialDNA
  
  selectFittest _ target population =
    -- we're only interested in the first, best match, but the API
    -- requires a list for the sake of the other implementation
    return $ take 1 $ sortByFitness target population
    
  breed ea [fittest] =
    -- we mutate every DNA sequence with the same (usually 5%) mutation rate
    -- we apply this after replicating the fittest member
    mutateAll (cMutationRate ea) $ replicate (cPopulation ea) fittest

-- Sexy
data SexualWeasel = SexyWeasel { sPopulation   :: Int
                               , sMutationRate :: Double
                               , sFitCutoff    :: Int }
                    deriving (Show, Eq)

defaultSexualWeasel = SexyWeasel { sPopulation = 100, sMutationRate = 0.05, sFitCutoff = 20 }

instance EvolutionaryAlgorithm SexualWeasel where
  generatePopulation ea length = 
    -- make a bunch of random DNA, for the total population
    sequence $ replicate (sPopulation ea) (randomDNA length)

  selectFittest ea target population =
    -- this is just like the "classic" one except it takes N instead of 1
    return $ take (sFitCutoff ea) $ sortByFitness target population

  breed ea fittest = do
    -- first, create all the partnerships and breed the number we
    -- need. Cycle is here to make the list infinitely long so we get
    -- additional pairings as needed (will depend on what the fitness
    -- cutoff number is)
    let pairings = take (sPopulation ea) $ cycle $ combinations 2 fittest

    -- perform the reproductions
    newPopulation <- mapM reproduceWith' pairings

    -- now effect mutation upon their hapless souls
    mutateAll (sMutationRate ea) newPopulation
      where
        reproduceWith' [x,y] = reproduceWith x y

-- | Perform a limited variation on sexual reproduction. Walk through
-- the DNA string for both parents and randomly choose one parent to
-- be the contributor for the child.
reproduceWith :: DNA -> DNA -> IO DNA
reproduceWith male female = zipWithM chooseEither male female
  where
    chooseEither :: Char -> Char -> IO Char
    chooseEither m f = do
      cond <- randomRIO (0,1) :: IO Int
      return $ case cond of
        0 -> f
        1 -> m
