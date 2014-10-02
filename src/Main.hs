module Main where

import Control.Monad (void)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)

import Util
import Classic
import Sexual

-- | Mode handling simplifies the command line parsing situation
-- later.
data Mode = ClassicMode | SexualMode | HelpMode
          deriving (Show, Read, Eq)

-- | Represents all the various information you can supply on the
-- command line.
data Parameters = Param { pTotalPopulation :: Int
                        , pTarget          :: DNA
                        , pFitCutoff       :: Int
                        , pMutationRate    :: Double
                        , pMode            :: Mode
                        } deriving (Show)

-- | Default values used when the user doesn't override them.
defaultParameters :: Parameters
defaultParameters = Param { pTotalPopulation = 100
                          , pTarget          = "METHINKS IT IS LIKE A WEASEL"
                          , pFitCutoff       = 20
                          , pMutationRate    = 0.05
                          , pMode            = ClassicMode
                          }

-- | The GetOpt command line options structure. We return a function
-- that returns a changed parameter according to whatever option was
-- selected. This is fed into a fold at the end.
commandLineOptions :: [OptDescr (Parameters -> Parameters)]
commandLineOptions = [
  Option "h" ["help"]
    (NoArg helpOption)
    "show this help, then exit",
  Option "p" ["population"]
    (ReqArg populationOption "POPULATION")
    "the number of organisms to keep per generation",
  Option "f" ["fit-cutoff"]
    (ReqArg cutoffOption "CUTOFF")
    "the number of organisms to consider 'fit' for reproduction",
  Option "r" ["mutation-rate"]
    (ReqArg mutationRateOption "RATE")
    "the mutation rate to use (e.g.: 0.05)",
  Option "c" ["classic-mode"]
    (NoArg  classicModeOption)
    "use classic mode",
  Option "s" ["sexy-mode"]
    (NoArg  sexyModeOption)
    "use sexual reproduction mode"]

-- These are the mutator functions
helpOption, classicModeOption, sexyModeOption :: Parameters -> Parameters
helpOption        opts = opts { pMode = HelpMode }
classicModeOption opts = opts { pMode = ClassicMode }
sexyModeOption    opts = opts { pMode = SexualMode }

-- These are mutator functions that incorporate string arguments from
-- the command line.
populationOption, cutoffOption :: String -> Parameters -> Parameters
mutationRateOption, modeOption :: String -> Parameters -> Parameters
populationOption   value p = p { pTotalPopulation = read     value } 
cutoffOption       value p = p { pFitCutoff       = read     value }
mutationRateOption value p = p { pMutationRate    = read     value }
modeOption         value p = p { pMode            = read     value }

-- | A convenience function to display help
showHelp :: IO ()
showHelp = do
  prog <- getProgName
  putStrLn $ usageInfo ("Usage: " ++ prog ++ " [OPTIONS]... [TARGET STRING]")
    commandLineOptions
  exitSuccess

-- | Convenience function to handle command line parsing.
parseOptions :: [String] -> IO Parameters
parseOptions args = do
  case getOpt RequireOrder commandLineOptions args of
    (opts, args',   []) -> return $ interpretOptions opts args'
    (   _,     _, errs) -> putStrLn (concat errs) >> exitFailure

-- | Convenience function to help with option/argument interpretation.
interpretOptions :: [(Parameters -> Parameters)] -> [String] -> Parameters
interpretOptions opts args = processedParameters { pTarget = target }
  where
    -- the fold here applies each of the selected parameter mutators
    -- in turn to the default parameters to make them into the
    -- parameters the user selected
    processedParameters = foldl (flip ($)) defaultParameters opts

    -- if the user supplied arguments, combine them all into nice new
    -- target string; otherwise keep the default
    target = if args == []
               then pTarget defaultParameters
               else sanitize $ unwords args

-- | Executes the appropriate computation based on the command line
-- argument structure that was given to us.
execute :: Parameters -> IO ()
execute (Param {pMode = HelpMode}) = showHelp
execute options@(Param {pTarget = target, pMode = ClassicMode}) =
  void $ weaselEvolver (classicOptions options) target
execute options@(Param {pTarget = target, pMode = SexualMode})  = 
  void $ weaselEvolver (sexualOptions options)  target

-- | Convert the parameters to a ClassicWeasel simulation
classicOptions :: Parameters -> ClassicWeasel
classicOptions options@(Param {pTotalPopulation = pop,
                               pMutationRate    = rate}) =
  ClassicWeasel { cPopulation = pop, cMutationRate = rate }

-- | Convert the parameters to a SexualWeasel simulation
sexualOptions :: Parameters -> SexualWeasel
sexualOptions options@(Param {pTotalPopulation = pop,
                              pMutationRate    = rate,
                              pFitCutoff       = fit}) =
  SexyWeasel { sPopulation = pop, sMutationRate = rate, sFitCutoff = fit }

-- Parse the input and then hand it off to the execute method.
main = do
  args <- getArgs
  options <- parseOptions args
  execute options
