package net.vlax.aoc2016.sol

import scala.io.BufferedSource

class Day03(src: BufferedSource) extends Solution:
  private def parseInput(src: BufferedSource): Array[Array[Int]] =
    import scala.util.matching.Regex

    val re = raw"(\d+)\s+(\d+)\s+(\d+)".r.unanchored

    src
      .getLines()
      .map({
        case re(n1, n2, n3) => Array(n1.toInt, n2.toInt, n3.toInt)
        case line => throw new RuntimeException(s"Invalid data: $line")
      })
      .toArray

  private lazy val triangles = parseInput(src)

  private def isTriangle(arr: Array[Int]): Boolean =
    arr(0) + arr(1) > arr(2) && arr(1) + arr(2) > arr(0) && arr(2) + arr(0) > arr(1)

  def partOne(): String =
    "%d".format(triangles.count(isTriangle))

  def partTwo(): String =
    "%d".format(triangles.grouped(3).flatMap(_.transpose).count(isTriangle))

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
