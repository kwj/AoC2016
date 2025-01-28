package net.vlax.aoc2016.sol

import scala.io.BufferedSource

class Day04(src: BufferedSource) extends Solution:
  private class roomInfo(val encName: String, val sectorId: Int, val checkSum: String):
    def isValidCheckSum() =
      encName.toArray
        .filter(_ != '-')
        .groupBy(identity)
        .map((k, v) => (k, v.size)) // (character, number of characters)
        .toSeq
        .sortBy({case (k, v) => (-v, k)})
        .take(5)
        .map(_(0))
        .mkString
        == checkSum

    def decode() =
      encName
        .map(ch =>
          ch match
            case '-' => ' '
            case _ => ((ch + sectorId - 'a') % 26 + 'a').toChar
        )
        .mkString

  private def parseInput(src: BufferedSource) =
    import scala.util.matching.Regex

    val re = raw"(\S+)-(\d+)\[(\S+)\]".r

    src
      .getLines()
      .map(line =>
        line match
          case re(encName, sectorId, checkSum) => roomInfo(encName, sectorId.toInt, checkSum)
          case _ => throw new RuntimeException(s"Invalid data: $line")
      )
      .toSeq

  private val rooms = parseInput(src)

  private def isNorthPoleObject(s: String) =
    import scala.util.matching.Regex

    val re1 = raw"north".r.unanchored
    val re2 = raw"pole".r.unanchored
    val re3 = raw"object".r.unanchored

    re1.matches(s) && re2.matches(s) && re3.matches(s)

  def partOne(): String =
    "%d".format(rooms.filter(_.isValidCheckSum()).map(_.sectorId).sum)

  def partTwo(): String =
    rooms.filter(_.isValidCheckSum()).find(elm => isNorthPoleObject(elm.decode())) match
      case Some(room) => "%d".format(room.sectorId)
      case None => throw new RuntimeException("There is no answer.")

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
