package net.vlax.aoc2016.sol

import scala.io.BufferedSource

class Day06(src: BufferedSource) extends Solution:
  private val messages = src.getLines().map(_.toArray).toArray.transpose

  def partOne(): String =
    messages
      .map(_.groupBy(identity).toSeq.maxBy(_(1).size))
      .map(_(0))
      .mkString

  def partTwo(): String =
    messages
      .map(_.groupBy(identity).toSeq.minBy(_(1).size))
      .map(_(0))
      .mkString

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
