package net.vlax.aoc2016.sol

import scala.io.BufferedSource

class Day24(src: BufferedSource) extends Solution:
  private def parseInput(src: BufferedSource): (Array[Array[Int]], Int) =
    val lines = src.getLines().map(_.toArray).toArray
    val width = lines(0).length
    val grid = lines.flatMap(identity)

    getDistanceTbl(grid, width)

  private def getDistanceTbl(grid: Array[Char], width: Int): (Array[Array[Int]], Int) =
    import scala.collection.mutable.{HashSet, Queue}

    val locations =
      grid.zipWithIndex.filter((ch, _) => ch.isDigit).map((ch, idx) => (ch.toInt - '0', idx))
    val distanceTbl = Array.fill(locations.length, locations.length)(0)

    for (src, start) <- locations
    do
      val visited = HashSet(start)
      val q = Queue((start, 0))
      while !q.isEmpty do
        val (pos, step) = q.dequeue
        if grid(pos).isDigit then
          val dst = grid(pos) - '0'
          distanceTbl(src)(dst) = step

        Seq(1, -1, width, -width)
          .map(_ + pos)
          .filter(x => grid(x) != '#')
          .filter(x => !visited.contains(x))
          .foreach(x =>
            visited.add(x)
            q.enqueue((x, step + 1))
          )

    (distanceTbl, locations.length)

  private lazy val (adjTbl, nLocs) = parseInput(src)

  private def classifySbs(n: Int, bitSet: Int): (Seq[Int], Seq[Int]) =
    (0 until n)
      .foldLeft((Seq.empty[Int], Seq.empty[Int]))((acc, x) =>
        val (visited, unVisited) = acc
        (bitSet & (1 << x)) != 0 match
          case true => (visited.appended(x), unVisited)
          case false => (visited, unVisited.appended(x))
      )

  private def runDP(adjTbl: Array[Array[Int]], n: Int): Array[Double] =
    val dpTbl = Array.fill(1 << n, n)(Double.PositiveInfinity)
    dpTbl(1)(0) = 0 // starting from '0'

    for
      bitSet <- (1 until (1 << n))
      (visited, unVisited) = classifySbs(n, bitSet)
    do
      unVisited
        .foreach(u =>
          val targetRow = bitSet | (1 << u)
          dpTbl(targetRow)(u) = visited.foldLeft(dpTbl(targetRow)(u))((acc, v) =>
            acc.min(dpTbl(bitSet)(v) + adjTbl(v)(u))
          )
        )

    dpTbl((1 << n) - 1)

  def partOne(): String =
    "%d".format(runDP(adjTbl, nLocs).min.toInt)

  def partTwo(): String =
    val dists = runDP(adjTbl, nLocs)
    (1 until nLocs).foreach(idx => dists(idx) += adjTbl(idx)(0))

    "%d".format(dists.min.toInt)

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
