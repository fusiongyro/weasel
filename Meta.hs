module Meta where

import Control.Monad.Reader
import Data.Char
import Data.Function
import Data.List
import System.Random

-- Meta-weasel

type DNA = String

data Mode = ClassicMode | SexualReproductionMode | HelpMode
          deriving (Show, Read, Eq)

data Parameters = Param { pTotalPopulation :: Int
                        , pTarget          :: DNA
                        , pFitCutoff       :: Int
                        , pMutationRate    :: Double
                        , pMode            :: Mode
                        } deriving (Show)

defaultParameters :: Parameters
defaultParameters = Param { pTotalPopulation = 100
                          , pTarget          = "METHINKS IT IS LIKE A WEASEL"
                          , pFitCutoff       = 20
                          , pMutationRate    = 0.05
                          , pMode            = ClassicMode
                          }

type Weasel = ReaderT Parameters IO

totalPopulation :: Weasel Int
totalPopulation = asks pTotalPopulation

target :: Weasel DNA
target = asks pTarget

fitCutoff :: Weasel Int
fitCutoff = asks pFitCutoff

mutationRate :: Weasel Double
mutationRate = asks pMutationRate

mode :: Weasel Mode
mode = asks pMode

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

randomDNA :: Weasel DNA
randomDNA = do
  t <- target
  lift $ randomString $ length t
