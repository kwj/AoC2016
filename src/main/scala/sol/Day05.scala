package net.vlax.aoc2016.sol

import scala.io.BufferedSource

class Day05(src: BufferedSource) extends Solution:
  private val doorId = src.getLines().toSeq.head

  private def intToHexChar(x: Int): Char =
    val tbl = "0123456789abcdef".toArray
    x match
      case idx if idx <= 0x0f => tbl(x)
      case _ => throw new RuntimeException(f"Invalid data: $x%d is larger than 15")

  private def hashGenerator(seed: String): Iterator[Array[Byte]] =
    import java.security.MessageDigest

    val md5 = MessageDigest.getInstance("MD5")
    md5.update(seed.getBytes())

    Iterator
      .from(0)
      .map(n => md5.clone().asInstanceOf[MessageDigest].digest(n.toString().getBytes()))
      .filter(arr => arr(0) == 0 && arr(1) == 0 && (arr(2) & 0xf0) == 0)

  def partOne(): String =
    hashGenerator(doorId)
      .take(8)
      .map(hashArr => intToHexChar(hashArr(2) & 0x0f))
      .mkString

  def partTwo(): String =
    hashGenerator(doorId)
      .map(hashArr => (hashArr(2) & 0x0f, (hashArr(3) & 0xf0) >> 4))
      .scanLeft(Array.fill(8)('_'))((passwdArr, tpl) =>
        val (idx, v) = tpl
        if idx < passwdArr.length && passwdArr(idx) == '_' then passwdArr(idx) = intToHexChar(v)
        passwdArr
      )
      .find(_.forall(_ != '_'))
      .get
      .mkString

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
