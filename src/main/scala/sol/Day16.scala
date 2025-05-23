package net.vlax.aoc2016.sol

// Note that this solution assumes that the size of the space to be filled is even.

/*
$ factor 272
272: 2 2 2 2 17
$ factor 35651584
35651584: 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 17

It is clear that the results will be strings of 17-characters in length.

272 = 2 ^ 4 * 17
  block size = 2^4 = 16
  number of blocks = 17
35651584 = 2 ^ 21 * 17
  block size = 2^21 = 2097152
  number of blocks = 17

... -> 00, 11 -> 1
... -> 01, 10 -> 0

Each block is reduced to a single byte character ('0' or '1').
As a result of the reduction, this character becomes the odd parity of a block.

From example:
  110010110100:
    length = 12 = 2^2 * 3  (3 blocks)
      -> 1100 1011 0100
      -> 1100(1) 1011(0) 0100(0)   // (#) odd parity
      -> 1 0 0

  10000011110010000111
    length = 20 = 2^2 * 5  (5 blocks)
      -> 1000 0011 1100 1000 0111
      -> 1000(0) 0011(1) 1100(1) 1000(0) 0111(0)   // (#) odd parity
      -> 0 1 1 0 0

We only need to know whether the number of ones in each block is even or odd.


Directly finding the number of ones in each block would need to generate data
by a modified dragon curve. However, in case of blocks which start from the beginning
of the disk, there is an easy way to find it without actual data.

If the length of the block is less than or equal to the length of the input data,
it is easy to find it. Otherwise,

     len = k
^^^^^^^^^^^^^^^^^^
              len = 2k + 1
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
001100...0001...10 0 10...0111...110011
|<--- block (len = BS) --->||<--   -->|
         [BS > k]        len = 2k + 1 - BS

divide the block into two blocks as follows:

001100...0001...10 0 10...0111...110011
|<-sub/1->||<--  sub/2 --->|
len=2k+1-BS.     .
           |     |
           ^^^^^^^
        len=k-(2k+1-BS)

number of ones in the block
  = number of ones in the sub/1 + number of ones in the sub/2
  = number of ones in the sub/1 + (k - len(sub/1))

Conclusion:

let f(x) be a function that returns the number of ones in a block of size x:

  f(x) | = f(2k + 1 - x) + (k - (2k + 1 - x))   [when x > len(input data), and let a number k satisfies k < x <= 2k + 1]
       | = it's easy to know                    [when x <= len(input data)]
 */

import scala.io.BufferedSource

class Day16(src: BufferedSource) extends Solution:
  private def parseInput(src: BufferedSource) =
    val cumSum = src.getLines().next().split("").map(_.toInt).scan(0)(_ + _)
    (cumSum.length - 1, cumSum)

  private lazy val (inputLen, cumSum) = parseInput(src)

  private def nOnesInRange(rangeSize: Int): Int =
    def aux(k: Int, bs: Int, acc: Int): Int =
      bs <= inputLen match
        case true => acc + cumSum(bs)
        case false =>
          if k >= bs then aux(k >> 1, bs, acc)
          else
            val nextBS = (k << 1) + 1 - bs
            aux(k, nextBS, acc + (k - nextBS))

    val initK = Iterator.iterate(inputLen)(x => (x << 1) + 1).dropWhile(_ < rangeSize).next()

    aux(initK, rangeSize, 0)

  private def getCheckSum(targetSize: Int): String =
    def aux(blkSize: Int, len: Int): (Int, Int) =
      if (len & 0b1) == 0 then aux(blkSize << 1, len >> 1)
      else (blkSize, len)

    // targetSize must be a even number
    assert((targetSize & 0b1) == 0)

    // targetSize = blkSize(2^n) * nBlks
    val (blkSize, nBlks) = aux(1, targetSize)

    // idx    value
    //  0     0
    //  1     number of ones in the 1st block
    //  2     number of ones in the 1st and 2nd blocks
    // ...
    // nBlks  number of ones in the 1st, 2nd, ..., (nBlks-1)-th and nBlks-th blocks
    val onesCnt = (0 to nBlks).map(_ * blkSize).map(nOnesInRange)

    onesCnt
      .drop(1)
      .zip(onesCnt)
      .map(_ - _) // number of ones in each block
      .map(x => if (x & 0b1) == 0 then 1 else 0) // odd parity of each block
      .mkString

  def partOne(): String =
    getCheckSum(272)

  def partTwo(): String =
    getCheckSum(35651584)

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
