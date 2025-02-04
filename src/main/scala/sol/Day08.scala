package net.vlax.aoc2016.sol

import scala.io.BufferedSource

class Day08(src: BufferedSource) extends Solution:
  private sealed trait Instruction
  private case class Rect(nRow: Int, nCol: Int) extends Instruction
  private case class RotateRow(idx: Int, n: Int) extends Instruction
  private case class RotateCol(idx: Int, n: Int) extends Instruction

  private def parseInput(src: BufferedSource): Seq[Instruction] =
    import scala.util.matching.Regex

    val reRect = raw"rect (\d+)x(\d+)".r
    val reRow = raw"rotate row y=(\d+) by (\d+)".r
    val reCol = raw"rotate column x=(\d+) by (\d+)".r

    src
      .getLines()
      .map({
        case reRect(nCol, nRow) => Rect(nRow.toInt, nCol.toInt)
        case reRow(idx, nShift) => RotateRow(idx.toInt, nShift.toInt)
        case reCol(idx, nShift) => RotateCol(idx.toInt, nShift.toInt)
        case line => throw new RuntimeException(s"Invalid instruction: $line")
      })
      .toSeq

  private lazy val instructions = parseInput(src)

  private class Grid(val nRow: Int, val nCol: Int):
    private val grid = Array.fill(nRow)(Array.fill(nCol)('.'))

    def op(inst: Rect): Unit =
      for
        r <- 0 until inst.nRow
        c <- 0 until inst.nCol
      do grid(r)(c) = '#'

    private def circleShiftRight(seq: Array[Char], n: Int): Array[Char] =
      val len = seq.size
      seq.drop(len - (n % len)) ++ seq.take(len - (n % len))

    def op(inst: RotateRow): Unit =
      val newSeq = circleShiftRight(grid(inst.idx), inst.n)
      newSeq.copyToArray(grid(inst.idx))

    def op(inst: RotateCol): Unit =
      val newSeq = circleShiftRight(grid.map(_(inst.idx)), inst.n)
      for r <- 0 until nRow do grid(r)(inst.idx) = newSeq(r)

    def countUp(): Int = grid.map(_.count(_ == '#')).sum

    def display(): String = grid.map(_.mkString).mkString("\n")

  private inline val Height = 6
  private inline val Width = 50

  private def execOperations(): Grid =
    val g = Grid(Height, Width)

    instructions.foreach(inst =>
      inst match
        case Rect(_, _) => g.op(inst.asInstanceOf[Rect])
        case RotateRow(_, _) => g.op(inst.asInstanceOf[RotateRow])
        case RotateCol(_, _) => g.op(inst.asInstanceOf[RotateCol])
    )
    g

  def partOne(): String =
    val screen = execOperations()

    "%d".format(screen.countUp())

  def partTwo(): String =
    val screen = execOperations()

    screen.display()

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
