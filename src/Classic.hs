module Classic where

import Util

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
