package net.vlax.aoc2016.sol

// Note that each number of positions on disk is all different prime number.

import scala.io.BufferedSource

class Day15(src: BufferedSource) extends Solution:
  private def parseInput(src: BufferedSource): Seq[(Int, Int, Int)] =
    import scala.util.matching.Regex

    val reLine = raw"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).".r

    src
      .getLines()
      .map({
        case reLine(disk, nPositions, start) => (disk.toInt, nPositions.toInt, start.toInt)
        case line => throw new RuntimeException(s"Invalid format: $line")
      })
      .toSeq

  private lazy val arrangement = parseInput(src)

  private def xgcd(a: Int, b: Int): (Int, Int, Int) =
    if b == 0 then (1, 0, a)
    else
      val (x, y, d) = xgcd(b, a % b)
      (y, -(a / b) * y + x, d)

  private def invmod(a: Int, m: Int): Int =
    val (invX, _, _) = xgcd(a, m)
    if invX < 0 then invX + m else invX

  private def mod(a: Int, m: Int): Int =
    val x = a % m
    if x < 0 then x + m else x

  private def preExec(data: Seq[(Int, Int, Int)]): (Seq[Int], Seq[Int]) =
    data.map((disk, nPos, start) => (mod((-disk - start), nPos), nPos)).unzip

  // T ≡ b0 (mod. m0), T ≡ b1 (mod. m1), ...
  private def garner(b: Seq[Int], m: Seq[Int], modulo: Int) =
    val bArr = b.toArray
    val mArr = m.appended(modulo).toArray

    // mProducts[k] | = 1  (k = 0)
    //              | = m[0] * m[1] * ... * m[k-1]  (k > 0)
    val mProducts = Array.fill(mArr.length)(1)

    // T = x0 + x1 * m0 + x2 * m0 * m1 + ... + x{k} * m0 * m1 * ... * m{k-1} + ...  (mod. ###)
    // results[k] | = 0 [not used]  (k = 0)
    //            | = x0  (mod. ###)  (k = 1)
    //            | = x0 + ... + x{k-1} * m0 * ... * m{k-2}  (mod. ###)  (k >= 2)
    val results = Array.fill(mArr.length)(0)

    for i <- bArr.indices
    do
      val t = mod((bArr(i) - results(i)) * invmod(mProducts(i), m(i)), mArr(i))
      for j <- Range(i + 1, mArr.length)
      do
        results(j) = mod(results(j) + t * mProducts(j), mArr(j))
        mProducts(j) = mod(mProducts(j) * mArr(i), mArr(j))

    results.last

  def partOne(): String =
    val (b, m) = preExec(arrangement)
    "%d".format(garner(b, m, m.fold(1)(_ * _)))

  def partTwo(): String =
    val (b, m) = preExec(arrangement)
    val newB = b.appended(mod((-(b.length + 1) - 0), 11))
    val newM = m.appended(11)
    "%d".format(garner(newB, newM, newM.fold(1)(_ * _)))

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
