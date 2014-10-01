module Sexual where

import Control.Monad (zipWithM)
import System.Random (randomRIO, randomIO)

import Util

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
