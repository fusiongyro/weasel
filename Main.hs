module Main where

import Meta
import Classic

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import System.Console.GetOpt
import System.Environment
import System.Exit

commandLineOptions :: [OptDescr (Parameters -> Parameters)]
commandLineOptions = [
  Option "h" ["help"]          (NoArg  helpOption)                      "show this help, then exit",
  Option "p" ["population"]    (ReqArg populationOption   "POPULATION") "the number of organisms to keep per generation",
  Option "t" ["target"]        (ReqArg targetOption       "TARGET")     "the target string of DNA to shoot for",
  Option "f" ["fit-cutoff"]    (ReqArg cutoffOption       "CUTOFF")     "the number of organisms to consider 'fit' for reproduction",
  Option "r" ["mutation-rate"] (ReqArg mutationRateOption "RATE")       "the mutation rate to use (e.g.: 0.05)",
  Option "c" ["classic-mode"]  (NoArg  classicModeOption)               "use classic mode",
  Option "s" ["sexy-mode"]     (NoArg  sexyModeOption)                  "use sexual reproduction mode"]

helpOption, classicModeOption, sexyModeOption :: Parameters -> Parameters
helpOption        opts = opts { pMode = HelpMode }
classicModeOption opts = opts { pMode = ClassicMode }
sexyModeOption    opts = opts { pMode = SexualReproductionMode }

showHelp :: IO ()
showHelp = do
  prog <- getProgName
  putStrLn $ usageInfo ("Usage: " ++ prog ++ " [OPTIONS]...") commandLineOptions
  exitSuccess

populationOption, targetOption, cutoffOption, mutationRateOption, modeOption :: String -> Parameters -> Parameters
populationOption   value p = p { pTotalPopulation = read value } 
targetOption       value p = p { pTarget          = value      }
cutoffOption       value p = p { pFitCutoff       = read value }
mutationRateOption value p = p { pMutationRate    = read value }
modeOption         value p = p { pMode            = read value }

parseOptions :: [String] -> IO Parameters
parseOptions args = do
  case getOpt RequireOrder commandLineOptions args of
    (opts, args',   []) -> return $ foldl (flip ($)) defaultParameters opts
    (   _,     _, errs) -> putStrLn (concat errs) >> exitFailure

main = do
  args <- getArgs
  options <- parseOptions args
  when (pMode options == HelpMode) showHelp
  runReaderT (randomDNA >>= simulate 1) options

