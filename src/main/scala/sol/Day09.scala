package net.vlax.aoc2016.sol

import scala.io.BufferedSource

class Day09(src: BufferedSource) extends Solution:
  import scala.util.matching.Regex

  private val reMarker = raw"\((\d+)x(\d+)\)".r

  private lazy val message = src.getLines().mkString

  private def deCompLength1(s: String): Long =
    reMarker.findFirstMatchIn(s) match
      case None => s.length().toLong
      case Some(m) =>
        val len = m.group(1).toInt
        val nCount = m.group(2).toInt

        m.start
          + (len * nCount).toLong
          + deCompLength1(m.after.toString().substring(len))

  private def deCompLength2(s: String): Long =
    reMarker.findFirstMatchIn(s) match
      case None => s.length().toLong
      case Some(m) =>
        val len = m.group(1).toInt
        val nCount = m.group(2).toLong
        val restStr = m.after.toString()
        if len > restStr.length() then
          throw new RuntimeException("Other algorithms are needed to solve this problem")

        m.start
          + deCompLength2(restStr.substring(0, len)) * nCount
          + deCompLength2(restStr.substring(len))

  def partOne(): String =
    "%d".format(deCompLength1(message))

  def partTwo(): String =
    "%d".format(deCompLength2(message))

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
