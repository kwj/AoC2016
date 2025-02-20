package net.vlax.aoc2016.sol

/*
[Part 1]
Josephus problem
f(2n) = 2f(n) - 1
f(2n + 1) = 2f(n) + 1

f(n) = 2x + 1  when n = 2^m + x (0 <= x < 2^m)

[Part 2]
a ... Winner
start(0-base), nElves - elves
--------
0, 1  - [a]  (*)
1, 2  - b [a]  (*)
0, 3  - [b] c a
3, 4  - b d c [a]  (*)
3, 5  - e b d [c] a
3, 6  - f e b [d] c a  (*)
2, 7  - f e [b] d c g a
1, 8  - f [e] b d c h g a
0, 9  - [f] e b d i c h g a
9, 10 - f e b d j i c h g [a]  (*)
9, 11 - f e b k d j i c h [g] a
 ...
 */

import scala.io.BufferedSource

class Day19(src: BufferedSource) extends Solution:
  private lazy val targetNumberOfElves = src.getLines().next().toInt

  def partOne(): String =
    import scala.math.log

    val mask = (1 << (log(targetNumberOfElves) / log(2)).ceil.toInt) - 1
    "%d".format(((targetNumberOfElves << 1) + 1) & mask)

  def partTwo(): String =
    def aux(startPos: Int, nElves: Int): Int =
      val remaining = targetNumberOfElves - nElves

      remaining > 0 match
        case true =>
          if startPos <= nElves / 2 then
            remaining <= startPos match
              case true =>
                aux(startPos - remaining, nElves + remaining)
              case false =>
                val nextNumElves = nElves + (startPos + 1)
                aux(nextNumElves - 1, nextNumElves)
          else aux(startPos, nElves + (startPos - 1).min(remaining))
        case false =>
          targetNumberOfElves - startPos

    "%d".format(aux(0, 1))

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
