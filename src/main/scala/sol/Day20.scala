package net.vlax.aoc2016.sol

import scala.io.BufferedSource

class Day20(src: BufferedSource) extends Solution:
  private case class DropRange(start: Long, end: Long)

  private def parseInput(src: BufferedSource): Seq[DropRange] =
    import scala.util.matching.Regex

    val reRange = raw"(\d+)-(\d+)".r

    src
      .getLines()
      .map({
        case reRange(start, stop) => DropRange(start.toLong, stop.toLong)
        case line => throw new RuntimeException(s"Invalid line: $line")
      })
      .toSeq
      .sortBy(_.start)

  private lazy val dropRanges = parseInput(src)

  def partOne(): String =
    def aux(ranges: Seq[DropRange], lowestIP: Long): Long =
      ranges.headOption match
        case Some(dropRange) =>
          if lowestIP < dropRange.start then lowestIP
          else aux(ranges.tail, lowestIP.max(dropRange.end + 1))
        case None => lowestIP

    "%d".format(aux(dropRanges, 0))

  def partTwo(): String =
    def aux(ranges: Seq[DropRange], startIP: Long, acc: Long): Long =
      ranges.headOption match
        case Some(dropRange) =>
          dropRange.start - startIP match
            case n if n > 0 => aux(ranges.tail, startIP.max(dropRange.end + 1), acc + n)
            case _ => aux(ranges.tail, startIP.max(dropRange.end + 1), acc)
        case None => acc

    "%d".format(aux(dropRanges, 0, 0))

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
