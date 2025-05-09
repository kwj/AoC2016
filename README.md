# Advent of Code 2016

URL: https://adventofcode.com/2016

## Prerequisites

* [Scala](https://www.scala-lang.org/) (confirmed to work with Scala 3.7.0)
* [sbt](https://www.scala-sbt.org/) (confimed to work with sbt 1.10.11)

## Solutions

* [Day 1: No Time for a Taxicab](./src/main/scala/sol/Day01.scala)
* [Day 2: Bathroom Security](./src/main/scala/sol/Day02.scala)
* [Day 3: Squares With Three Sides](./src/main/scala/sol/Day03.scala)
* [Day 4: Security Through Obscurity](./src/main/scala/sol/Day04.scala)
* [Day 5: How About a Nice Game of Chess?](./src/main/scala/sol/Day05.scala)
* [Day 6: Signals and Noise](./src/main/scala/sol/Day06.scala)
* [Day 7: Internet Protocol Version 7 ](./src/main/scala/sol/Day07.scala)
* [Day 8: Two-Factor Authentication](./src/main/scala/sol/Day08.scala)
* [Day 9: Explosives in Cyberspace](./src/main/scala/sol/Day09.scala)
* [Day 10: Balance Bots](./src/main/scala/sol/Day10.scala)
* [Day 11: Radioisotope Thermoelectric Generators](./src/main/scala/sol/Day11.scala)
* [Day 12: Leonardo's Monorail](./src/main/scala/sol/Day12.scala)
* [Day 13: A Maze of Twisty Little Cubicles](./src/main/scala/sol/Day13.scala)
* [Day 14: One-Time Pad](./src/main/scala/sol/Day14.scala)
* [Day 15: Timing is Everything](./src/main/scala/sol/Day15.scala)
* [Day 16: Dragon Checksum](./src/main/scala/sol/Day16.scala)
* [Day 17: Two Steps Forward](./src/main/scala/sol/Day17.scala)
* [Day 18: Like a Rogue](./src/main/scala/sol/Day18.scala)
* [Day 19: An Elephant Named Joseph](./src/main/scala/sol/Day19.scala)
* [Day 20: Firewall Rules](./src/main/scala/sol/Day20.scala)
* [Day 21: Scrambled Letters and Hash](./src/main/scala/sol/Day21.scala)
* [Day 22: Grid Computing](./src/main/scala/sol/Day22.scala)
* [Day 23: Safe Cracking](./src/main/scala/sol/Day23.scala)
* [Day 24: Air Duct Spelunking](./src/main/scala/sol/Day24.scala)
* [Day 25: Clock Signal](./src/main/scala/sol/Day25.scala)

## How to use

### Place puzzle input data files into each resource folder in advance

The default file name of the puzzle input data is `input`:

```console
$ ls ./src/main/resources/*/input
./src/main/resources/01/input  ./src/main/resources/02/input
./src/main/resources/03/input  ./src/main/resources/04/input
  ...
./src/main/resources/23/input  ./src/main/resources/24/input
./src/main/resources/25/input
$
```

## Run sbt
```console
$ sbt
...
[info] started sbt server
sbt:AoC2016>
```

## Run the program
```console
sbt:AoC2016> run
...
Please enter a date number, or Ctrl-D to exit.
>
```

## Enter the date
```console
> 1
***
***
Elapsed time: **** msec.

>
```

It is possible to specify an input file prepared with a different name.
```console
> 2 input-example
***
***
Elapsed time: **** msec.

>
```


## Note

There are no input data files in this repository.
Please get them from the AoC 2016 site.

See [here](https://adventofcode.com/about#faq_copying) for the reasons.
