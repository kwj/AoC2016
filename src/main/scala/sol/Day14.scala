package net.vlax.aoc2016.sol

import java.security.MessageDigest
import scala.io.BufferedSource

class Day14(src: BufferedSource) extends Solution:
  private inline val nth = 64
  private inline val nConsecutive = 1000

  private lazy val salt = src.getLines().next()

  private def toHexString(hash: Array[Byte]): String =
    val tbl = "0123456789abcdef".toArray
    val buf = new Array[Char](hash.length * 2)

    for
      i <- hash.indices
      data = hash(i) & 0xff
    do
      buf(i * 2) = tbl(data >> 4)
      buf(i * 2 + 1) = tbl(data & 0x0f)

    buf.mkString

  private def hashGenerator(seed: String): Iterator[String] =
    val md5 = MessageDigest.getInstance("MD5")
    md5.update(seed.getBytes())

    Iterator
      .from(0)
      .map(n => md5.clone().asInstanceOf[MessageDigest].digest(n.toString().getBytes()))
      .map(toHexString)

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

    val c3 = if c3s.isEmpty then 0 else 1 << c3s.next()
    val c5 = c5s.foldLeft(0)((acc, n) => acc | (1 << n))

    (c3, c5)

  private def searchKeyIndexes(seed: String, stretch: Int = 0): Iterator[(Int, Int, Int)] =
    val md5 = MessageDigest.getInstance("MD5")

    hashGenerator(seed)
      .map(hash =>
        Iterator.iterate(hash)(s => toHexString(md5.digest(s.getBytes()))).drop(stretch).next()
      )
      .zipWithIndex
      .map((s, idx) =>
        val (c3, c5) = checkConsecutive(s)
        (idx, c3, c5)
      )
      .sliding(1 + nConsecutive)
      .filter({ case (i, c3, _) +: rest => c3 != 0 && rest.exists((_, _, c5) => (c3 & c5) != 0) })
      .map(_.head)

  def partOne(): String =
    "%d".format(searchKeyIndexes(salt).drop(nth - 1).next()(0))

  def partTwo(): String =
    // Note that it took a few minutes on Raspberry Pi 4.
    "%d".format(searchKeyIndexes(salt, 2016).drop(nth - 1).next()(0))

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
