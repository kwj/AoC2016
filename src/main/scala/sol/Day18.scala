package net.vlax.aoc2016.sol

import scala.io.BufferedSource

class Day18(src: BufferedSource) extends Solution:
  private def parseInput(src: BufferedSource): (Int, BigInt, BigInt) =
    val line = src.getLines().next()
    val state =
      line
        .split("")
        .map({
          case "^" => BigInt(1)
          case _ => BigInt(0)
        })
        .fold(BigInt(0))((acc, x) => (acc << 1) + x)

    (line.length(), state, (BigInt(1) << line.length()) - 1)

  private lazy val (len, seed, mask) = parseInput(src)

  private def nSafeTiles(n: BigInt): Int = len - n.bitCount

  private def nextRow(n: BigInt): BigInt = ((n << 1) ^ (n >> 1)) & mask

  private def totalSafeTiles(nRows: Int): Int =
    Iterator
      .iterate(seed)(nextRow)
      .take(nRows)
      .map(nSafeTiles)
      .sum

  def partOne(): String =
    "%d".format(totalSafeTiles(40))

  def partTwo(): String =
    "%d".format(totalSafeTiles(400000))

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
