module Main where

import Control.Monad.Reader
import Data.Char
import Data.Function
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Random

-- Meta-weasel

type DNA = String

data Mode = ClassicMode | SexualReproductionMode
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

commandLineOptions :: [OptDescr (Parameters -> IO Parameters)]
commandLineOptions = [
  Option "h" ["help"]          (NoArg  showHelp)                        "show this help, then exit",
  Option "p" ["population"]    (ReqArg populationOption   "POPULATION") "the number of organisms to keep per generation",
  Option "t" ["target"]        (ReqArg targetOption       "TARGET")     "the target string of DNA to shoot for",
  Option "f" ["fit-cutoff"]    (ReqArg cutoffOption       "CUTOFF")     "the number of organisms to consider 'fit' for reproduction",
  Option "r" ["mutation-rate"] (ReqArg mutationRateOption "RATE")       "the mutation rate to use (e.g.: 0.05)",
  Option "m" ["mode"]          (ReqArg modeOption         "MODE")       "the mode to execute"]

showHelp :: Parameters -> IO Parameters
showHelp    opts = do
  prog <- getProgName
  putStrLn $ usageInfo ("Usage: " ++ prog ++ " [OPTIONS]...") commandLineOptions
  _ <- exitWith ExitSuccess
  return opts

populationOption, targetOption, cutoffOption, mutationRateOption, modeOption :: String -> Parameters -> IO Parameters
populationOption   value p = return $ p { pTotalPopulation = read value } 
targetOption       value p = return $ p { pTarget          = value      }
cutoffOption       value p = return $ p { pFitCutoff       = read value }
mutationRateOption value p = return $ p { pMutationRate    = read value }
modeOption         value p = return $ p { pMode            = read value }

parseOptions :: [String] -> IO Parameters
parseOptions args = do
  case getOpt RequireOrder commandLineOptions args of
    (opts, args',   []) -> foldl (>>=) (return defaultParameters) opts
    (   _,     _, errs) -> putStrLn (concat errs) >> exitFailure


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

main = do
  args <- getArgs
  options <- parseOptions args
  runReaderT (randomDNA >>= simulate 1) options

