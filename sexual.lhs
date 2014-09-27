> module Main where

> import Control.Monad
> import Data.Char
> import Data.Function
> import Data.List
> import System.Exit
> import System.Random
> import Text.EditDistance

This program is a variation of Dawkins's Weasel program.

> type DNA = String

> target :: DNA
> target = "METHINKS IT IS LIKE A WEASEL"

The idea is to demonstrate an evolutionary-like algorithm. I say
"like" because we know where we want to wind up. Henri Bergson pointed
out in _Creative Evolution_ that interpretations of evolution with a
target are not much like actual evolution, which doesn't have an
agenda per se, but let's ignore the problem for now.

In this program we conflate the creature and the DNA. In reality, we
would examine the creature for fitness; in the program, we examine the
DNA as a proxy for the creature. We could make this explicit, but
let's not for now.

For fun, we model the problem with fitness, mutation, and sexual
reproduction.

Fitness is simply defined as the edit distance between the DNA string
and the desired DNA string. This is kind of lame, but we take it.

Mutation is modeled by randomly changing elements of the DNA. For fun,
we consider a mutation rate of X% to mean that there is an X% chance
of any mutation happening on the string—each character has an (X/L)%
chance of mutating, where L is the length of the string. The mutation
will be the replacement of the character with a randomly selected
character. Nothing more sophisticated; no insertion, no deletion.

Sexual reproduction is achieved by walking through both DNA strings
simultaneously and flipping a coin to choose one parent or the other.

Inputs to the program are:

 - The "destination" string
 - The mutation rate
 - The population size per generation
 - The number of "fit" candidates to use to produce the next generation

The last two numbers are taken together to suggest how "fitness"
affects the likelihood of getting into the next generation. For
instance, if the population size matches the fit count, every member
of the current generation will produce two offspring for the next
generation, with two different partners. So in a 4/4 configuration,
A1, B1, C1, and D1 will mate like A1+B1=A2,A1+C1=B2,B1+D1=C2,C1+D1=D2.
In a 4/2 configuration, only the two fittest from each generation will
produce the next generation, so A1+B1=A2, A1+B1=B2, A1+B1=C2,
A1+B1=D2. All four of those came from the same two parents, but they
will only be identical if the mutation rate is 0%.

Outputs from the program are:

 - The generation "number" that produced the destination string
 - The "parents" that produced the destination string

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

This function is used to generate random DNA like so:

> randomDNA :: IO DNA
> randomDNA = randomString $ length target

Sexual reproduction is handled by the `reproduceWith` function.

> reproduceWith :: DNA -> DNA -> IO DNA
> male `reproduceWith` female = zipWithM chooseEither male female
>   where
>     chooseEither m f = do
>       cond <- randomRIO (0,20) :: IO Int
>       case cond of
>         0 -> randomChar 
>         n -> return $ if n `mod` 2 == 0 then f else m

Fitness is calculated by using the edit distance.

> fitness :: DNA -> Int
> fitness test = 28 - (sum $ map (\x -> if x == True then 1 else 0) $ zipWith (==) test target)

Now we can create a random population of size N.

> generatePopulation :: Int -> IO [DNA]
> generatePopulation n = sequence $ replicate n randomDNA

Once we have a population we can select the top N most fit.

> nMostFit :: Int -> [DNA] -> [DNA]
> nMostFit n population = take n $ sortBy (compare `on` fitness) population

Let's select the most-fit member too, just for fun during the output.

> mostFit :: [DNA] -> DNA
> mostFit population = (nMostFit 1 population) !! 0

> nextGeneration :: [DNA] -> IO [DNA]
> nextGeneration (x:y:xs) = do
>   new  <- x `reproduceWith` y
>   rest <- nextGeneration (y:xs)
>   return (new:rest)
> nextGeneration _ = return []

Now we can bundle it up and simulate a generation.

> simulateGeneration :: Int -> [DNA] -> IO [DNA]
> simulateGeneration decreaseTo population = do
>   let mostFit = nMostFit decreaseTo population
>   half1 <- nextGeneration mostFit
>   half2 <- nextGeneration mostFit
>   return $ half1 ++ half2

Now let's make the whole simulation and tie it together.

> simulation :: Int -> [DNA] -> IO ()
> simulation n population = do
>   let currentBest = mostFit population
>   putStrLn $ "Generation " ++ show n ++ ": " ++ currentBest ++ " (distance = " ++ show (fitness currentBest) ++ ")"
>   when (currentBest == target) exitSuccess
>   nextGen <- simulateGeneration 20 population
>   simulation (n+1) nextGen

Let's make the main:

> main = do
>   initialPopulation <- generatePopulation 100
>   simulation 1 initialPopulation
