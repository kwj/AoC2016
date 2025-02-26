package net.vlax.aoc2016.sol

/*
# The following is a Python implementation of the equivalent of the given program.

a = [Some initial value]
a += 2532

while True:
  d = a
  while d > 0:
    print(d % 2)
    d //= 2

----------------------
[Part 1]
Trying to solve this problem with simulation would also require a debugger-like implementation
to check outputs and registers. I was too lazy to do it, so I decided to read the given code and
got the answer directly.

As shown the above code, we need to find a number that its binary notation is clock signal.
 */

import scala.io.BufferedSource

class Day25(src: BufferedSource) extends Solution:
  private def parseInput(src: BufferedSource): Int =
    import scala.util.matching.Regex

    val reInt = raw"(\d+)".r.unanchored
    val lines = src.getLines().toArray

    val x = reInt.findFirstIn(lines(1)).get.toInt
    val y = reInt.findFirstIn(lines(2)).get.toInt

    x * y

  private lazy val baseNumber = parseInput(src)

  private def findValue(n: Int): Int =
    n >= baseNumber match
      case true => n - baseNumber
      case false => findValue((n << 2) | 0b10)

  def partOne(): String =
    "%d".format(findValue(0b10))

  def partTwo(): String =
    "There is nothing to do."

  def solve(): Unit =
    printf("%s\n", partOne())
