package net.vlax.aoc2016.sol

import scala.io.BufferedSource

class Day01(src: BufferedSource) extends Solution:
  private case class Pos2D(x: Int, y: Int):
    def +(other: Pos2D): Pos2D = Pos2D(x + other.x, y + other.y)
    def *(k: Int): Pos2D = Pos2D(x * k, y * k)
    def distance(): Int = x.abs + y.abs // manhattan distance from the starting position

  // north, east, south, west
  private val dirTbl: Array[Pos2D] = Array(Pos2D(0, 1), Pos2D(1, 0), Pos2D(0, -1), Pos2D(-1, 0))
  private val dirTblSize = dirTbl.length

  private def parseInput(src: BufferedSource): Seq[(Int, Int)] =
    import scala.util.matching.Regex

    val reL = raw"L(\d+)".r
    val reR = raw"R(\d+)".r

    src
      .getLines()
      .flatMap(_.split(", "))
      .map({
        case reL(n) => (-1, n.toInt)
        case reR(n) => (1, n.toInt)
        case token => throw new RuntimeException(s"Invalid instruction: $token")
      })
      .toSeq

  // [(direction of turn, distance), ...]
  //   direction -  1:right, -1:left
  private val instructions = parseInput(src)

  def partOne(): String =
    val (_, lastPos) =
      instructions
        .foldLeft((0, Pos2D(0, 0)))((state, inst) =>
          val newDir = (state(0) + inst(0) + dirTblSize) % dirTblSize
          (newDir, state(1) + (dirTbl(newDir) * inst(1)))
        )

    "%d".format(lastPos.distance())

  def partTwo(): String =
    val positions =
      instructions
        .scanLeft((0, List(Pos2D(0, 0))))((state, inst) =>
          val newDir = (state(0) + inst(0) + dirTblSize) % dirTblSize
          val start = state(1).last
          (newDir, Range(1, inst(1) + 1).toList.map(k => start + (dirTbl(newDir) * k)))
        )
        .flatMap(_(1))

    "%d".format(positions.diff(positions.distinct).head.distance())

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
