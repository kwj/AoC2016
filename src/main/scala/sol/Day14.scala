package net.vlax.aoc2016.sol

import java.security.MessageDigest
import scala.io.BufferedSource

class Day14(src: BufferedSource) extends Solution:
  private inline val nth = 64
  private inline val nConsecutive = 1000

  private lazy val salt = src.getLines().next()
  private val trTbl = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')

  private def toHexString(hash: Array[Byte]): String =
    val buf = new Array[Char](hash.length * 2)

    for
      i <- hash.indices
      data = hash(i) & 0xff
    do
      buf(i * 2) = trTbl(data >> 4)
      buf(i * 2 + 1) = trTbl(data & 0x0f)

    buf.mkString

  private def checkConsecutive(hash: String): (Int, Int) =
    def isAllSame(arr: Array[Char]): Boolean = arr.forall(_ == arr.head)

    def getConsecNumbers(arr: Array[Char], len: Int): Iterator[Int] =
      arr
        .sliding(len, 1)
        .filter(isAllSame)
        .map(x => Integer.parseInt(x(0).toString, 16))

    val data = hash.toArray
    val c3s = getConsecNumbers(data, 3)
    val c5s = getConsecNumbers(data, 5)

    (
      if c3s.isEmpty then 0 else 1 << c3s.next(), // Only the first triplet is valid
      c5s.foldLeft(0)((acc, n) => acc | (1 << n))
    )

  private def searchKeyIndexes(seed: String, nStretch: Int = 0): Iterator[Int] =
    val md5 = MessageDigest.getInstance("MD5")

    def stretch(hashStr: String, cnt: Int): String =
      cnt > 0 match
        case true => stretch(toHexString(md5.digest(hashStr.getBytes())), cnt - 1)
        case false => hashStr

    Iterator
      .from(0)
      .map(n => toHexString(md5.digest((seed + n.toString()).getBytes())))
      .map(s => stretch(s, nStretch))
      .zipWithIndex
      .map((s, idx) =>
        val (c3, c5) = checkConsecutive(s)
        (idx, s, c3, c5)
      )
      .sliding(1 + nConsecutive)
      .withFilter({ case (_, _, c3, _) +: rest => c3 != 0 && rest.exists((_, _, _, c5) => (c3 & c5) != 0) })
      .map(_.head)
      .map(_(0))

  def partOne(): String =
    "%d".format(searchKeyIndexes(salt).drop(nth - 1).next())

  def partTwo(): String =
    // Note that it took a few minutes on Raspberry Pi 4.
    "%d".format(searchKeyIndexes(salt, 2016).drop(nth - 1).next())

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
