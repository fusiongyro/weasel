module Main where

type DNA = String

class EvolutionaryAlgorithm a where
  generatePopulation :: a -> IO [DNA]
  selectFittest      :: a -> [DNA] -> IO [DNA]
  breed              :: a -> [DNA] -> IO [DNA]

weaselEvolver :: (EvolutionaryAlgorithm ea) => ea -> DNA -> IO Int
weaselEvolver ea target = do
  initialPopulation <- generatePopulation ea
  evolve 1 initialPopulation
  where
    evolve generation population = do
      fittest        <- selectFittest ea population
      nextPopulation <- breed ea fittest
      if head fittest == target
        then return generation
        else evolve (generation + 1) nextPopulation
  
-- Classic
data ClassicWeasel = ClassicWeasel { pPopulation   :: Int
                                   , pMutationRate :: Double }
                     deriving (Show, Eq)

defaultClassicWeasel = ClassicWeasel { pPopulation = 100, pMutationRate = 0.05 }

instance EvolutionaryAlgorithm ClassicWeasel where
  generatePopulation = undefined
  selectFittest = undefined
  breed = undefined

-- Sexy
