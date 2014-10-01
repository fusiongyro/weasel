# The Weasel Program

This program is an implementation of Richard Dawkins's famous [Weasel program](http://en.wikipedia.org/wiki/Weasel_program).

The concept behind this program is to demonstrate a simple kind of evolution. In addition to the classic Dawkins mode, it also implements a simple sexual reproduction mode. In sexual reproduction mode, two parents are blended to create the child by flipping a coin to choose one parent or the other for each character.

## Usage

To run the classic simulation, just run `./weasel`. This performs a “classic” simulation with a population of 100 and a mutation rate of 5% against the string `METHINKS IT IS LIKE A WEASEL`. You can change the target string with `-t`, the mutation rate with `-r` and the population size with `-p`.

    $ ./dist/build/weasel/weasel -h
    Usage: weasel [OPTIONS]...
      -h             --help                   show this help, then exit
      -p POPULATION  --population=POPULATION  the number of organisms to keep per generation
      -t TARGET      --target=TARGET          the target string of DNA to shoot for
      -f CUTOFF      --fit-cutoff=CUTOFF      the number of organisms to consider 'fit' for reproduction
      -r RATE        --mutation-rate=RATE     the mutation rate to use (e.g.: 0.05)
      -c             --classic-mode           use classic mode
      -s             --sexy-mode              use sexual reproduction mode

The sexual reproduction mode can be requested by passing `-s`. It respects the aforementioned options as well as `-f` to change the number of organisms considered “fit,” the default is 20.

## “Classic” Mode

In classic mode, `weasel` generates a random DNA string of the same length as the target string. It then copies that string 100 times to create the initial population. The population is then mutated and the most-fit member is selected. The process then repeats with the most-fit member until the most-fit member matches the target string.

The default mutation rate is 5%, which means that every character in all of the DNA strings has a 5% chance of becoming a randomly selected other character. (Actually, the effective mutation rate is really 5% * 26/27 = 4.8% because there is a chance that a character selected for mutation will wind up with the same character randomly selected. This does not seem to have a significant effect on the result.)

## Sexual Reproduction Mode

In sexual reproduction mode, `weasel` generates a population of random DNA strings. The population is then sorted by fitness. The fitness cutoff is then applied to cull the population. If the fitness cutoff is 20 (the default), the top 20 are retained and the lower 80 are discarded. Each surviving member is then bred with every other surviving member as many times as necessary to obtain the original population size. The breeding function in sexual reproduction mode is to walk both parents' DNA in sequence, randomly selecting one or the other's character. This process creates a new generation, which is then mutated according to the classic algorithm and with which the process is repeated until the most-fit member of the population is the target string.

## Experimental Observations

A tiny experiment like this requires a lot of fine print, and adding my own seems hardly necessary, but I was surprised by these results enough that I felt they deserved to be mentioned specially.

### Effective Mutation Rates

The 5% mutation rate does not seem to be as off-the-cuff as it seems in the problem description. The 5% rate produces useful results fairly quickly without preventing convergence. Even 10% seems to be high enough to make convergence unlikely. Note that with a 28-character string such as `METHINKS IT IS LIKE A WEASEL`, a 5% mutation rate will equate to 1.4 mutations per DNA sequence, which is enough to get some interesting variety, while a 10% mutation rate equates to 2.8. If you're one character from converging but your mutation rate is likely to change two characters, convergence is likely to be delayed.

### Fitness and Mutation

In an earlier version of the program I was using an algorithm that computes the Levenshtein edit distance between the DNA string and the target string. The population would often get stuck in a local optimum about one character from convergence. This is because the mutation algorithm had no way of inserting or deleting characters, but the fitness test expected it to be there. The problem was obvious once the program ran, but during design it was easy to talk myself into Levenshtein being “better” than adding up ones for each character that matches. Let this be a lesson to us all.

### Sexual Reproduction is Faster

I note that sexual reproduction mode seems to produce results faster in many cases than classic mode. I would like to do a more advanced statistical analysis of this and see if it is a real effect. This is more of a placeholder than a real observation.

### Local and Global Optimization

The premise of this program is that whoever is most-fit this generation is the parent of whoever will be most-fit next generation. Real evolution does not work this way: lots of not-particularly fit organisms survive due to luck or whatever. You could think of this algorithm as spanning just the extreme edge of the tree of organisms real evolution would produce. It's not clear to me how one could improve on this state of affairs without wasting a lot of CPU time on unfit organisms. It might be worth an experiment, though, to see if not limiting the population size at all between generations causes convergence to happen in fewer generations—and what the runtime effect is.


