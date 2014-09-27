> module Main where
>
> import Control.Monad
> import Data.Char
> import Data.List
> import Data.Function
> import System.Exit
> import System.Random


This is the "weasel" algorithm.

 1. Start with a random string of 28 characters.

 2. Make 100 copies of the string (reproduce).

 3. For each character in each of the 100 copies, with a probability of
    5%, replace (mutate) the character with a new random character.

 4. Compare each new string with the target string "METHINKS IT IS LIKE
    A WEASEL", and give each a score (the number of letters in the string
    that are correct and in the correct position).

 5. If any of the new strings has a perfect score (28), halt.
    Otherwise, take the highest scoring string, and go to step 2.

> type DNA = String

> target :: DNA
> target = "METHINKS IT IS LIKE A WEASEL"

The charFor function gives us the character for a random integer.

> charFor :: Int -> Char
> charFor 26 = ' '
> charFor i = (chr (ord 'A' + i))

The randomChar function gives us a random character in the range we want.

> randomChar :: IO Char
> randomChar = do
>   i <- randomRIO (0,26)
>   return $ charFor i

The `randomString` function generates a completely random string,
containing just the 26 uppercase alphabetic characters and the space.

> randomString :: Int -> IO String
> randomString 0         = return ""
> randomString n | n > 0 = do
>   c <- randomChar
>   s <- randomString (n-1)
>   return $ c : s

Step #1 is the randomDNA function:

> randomDNA :: IO DNA
> randomDNA = randomString $ length target

Step #2 is the reproduce function:

> reproduce :: DNA -> [DNA]
> reproduce s = replicate 100 s

Step #3 is the mutate function:

> mutate :: [DNA] -> IO [DNA]
> mutate = mapM mutateDNA
> 
> mutateDNA :: DNA -> IO DNA
> mutateDNA = mapM mutate'
>   where
>     mutate' char = do
>       cond <- randomRIO (1,20) :: IO Int
>       case cond of
>         1 -> randomChar 
>         _ -> return char

Here we deviate from the basic plan. We sort the strings by score. If
the score is 28, we finish. Otherwise, we take the highest scoring one
and use it to recur on the reproduction step.

> simulate :: Int -> DNA -> IO ()
> simulate n currentBest = do
>   putStrLn $ "Generation " ++ show n ++ ": " ++ currentBest ++ " (distance = " ++ show (fitness currentBest) ++ ")"
>   when (currentBest == target) exitSuccess
>   let population = reproduce currentBest
>   generation <- mutate population
>   simulate (n+1) $ best generation

> best :: [DNA] -> DNA
> best = last . sortBy (compare `on` fitness)

> fitness :: DNA -> Int
> fitness test = sum $ map (\x -> if x == True then 1 else 0) $ zipWith (==) test target

> main = do
>   initialDNA <- randomDNA
>   simulate 1 initialDNA
