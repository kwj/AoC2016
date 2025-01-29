package net.vlax.aoc2016.sol

import scala.io.BufferedSource

class Day07(src: BufferedSource) extends Solution:
  private def fork[T](lst: List[T]): (List[T], List[T]) =
    lst match
      case x :: xs =>
        val (ys, zs) = fork(xs)
        (x :: zs, ys)
      case Nil =>
        (Nil, Nil)

  private def parseInput(src: BufferedSource): Seq[(String, String)] =
    src
      .getLines()
      .map(_.split(Array('[', ']')).toList)
      .map(fork)
      .map((outer, inner) => (outer.mkString(" "), inner.mkString(" ")))
      .toSeq

  private val addressInfo = parseInput(src)

  def partOne(): String =
    import scala.util.matching.Regex

    val reABBA = raw"(\w)(?!\1)(\w)\2\1".r.unanchored

    val nTLS =
      addressInfo
        .filter((outer, inner) => reABBA.matches(outer) && !reABBA.matches(inner))
        .size

    "%d".format(nTLS)

  def partTwo(): String =
    import scala.util.matching.Regex

    val reABA = raw"(?=(\w)(?!\1)(\w)\1)".r

    val nSSL =
      addressInfo
        .filter((outer, inner) =>
          reABA
            .findAllMatchIn(outer)
            .exists(m => inner.contains(m.group(2) + m.group(1) + m.group(2)))
        )
        .size

    "%d".format(nSSL)

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
