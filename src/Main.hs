module Main where

import Control.Monad (void)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)

import Util
import Classic
import Sexual

data Mode = ClassicMode | SexualMode | HelpMode
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

commandLineOptions :: [OptDescr (Parameters -> Parameters)]
commandLineOptions = [
  Option "h" ["help"]
    (NoArg helpOption)
    "show this help, then exit",
  Option "p" ["population"]
    (ReqArg populationOption "POPULATION")
    "the number of organisms to keep per generation",
  Option "t" ["target"]
    (ReqArg targetOption "TARGET")
    "the target string of DNA to shoot for",
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

helpOption, classicModeOption, sexyModeOption :: Parameters -> Parameters
helpOption        opts = opts { pMode = HelpMode }
classicModeOption opts = opts { pMode = ClassicMode }
sexyModeOption    opts = opts { pMode = SexualMode }

showHelp :: IO ()
showHelp = do
  prog <- getProgName
  putStrLn $ usageInfo ("Usage: " ++ prog ++ " [OPTIONS]...") commandLineOptions
  exitSuccess

populationOption, targetOption, cutoffOption :: String -> Parameters -> Parameters
mutationRateOption, modeOption :: String -> Parameters -> Parameters
populationOption   value p = p { pTotalPopulation = read     value } 
targetOption       value p = p { pTarget          = sanitize value }
cutoffOption       value p = p { pFitCutoff       = read     value }
mutationRateOption value p = p { pMutationRate    = read     value }
modeOption         value p = p { pMode            = read     value }

parseOptions :: [String] -> IO Parameters
parseOptions args = do
  case getOpt RequireOrder commandLineOptions args of
    (opts, args',   []) -> return $ foldl (flip ($)) defaultParameters opts
    (   _,     _, errs) -> putStrLn (concat errs) >> exitFailure

main = do
  args <- getArgs
  options <- parseOptions args
  execute options

execute :: Parameters -> IO ()
execute (Param {pMode = HelpMode}) = showHelp
execute options@(Param {pTarget = target, pMode = ClassicMode}) =
  void $ weaselEvolver (classicOptions options) target
execute options@(Param {pTarget = target, pMode = SexualMode})  = 
  void $ weaselEvolver (sexualOptions options)  target

classicOptions options@(Param {pTotalPopulation = pop,
                               pMutationRate    = rate}) =
  ClassicWeasel { cPopulation = pop, cMutationRate = rate }

sexualOptions options@(Param {pTotalPopulation = pop,
                              pMutationRate    = rate,
                              pFitCutoff       = fit}) =
  SexyWeasel { sPopulation = pop, sMutationRate = rate, sFitCutoff = fit }
