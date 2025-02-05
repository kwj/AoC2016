package net.vlax.aoc2016.sol

import scala.collection.mutable.{HashSet, Queue}
import scala.io.BufferedSource

class Day13(src: BufferedSource) extends Solution:
  private case class Pos2D(x: Int, y: Int):
    def +(other: Pos2D): Pos2D = Pos2D(x + other.x, y + other.y)
    def isOpen(): Boolean =
      (Integer.bitCount((x + y) * (x + y + 1) + 2 * x + seed) & 0b1) == 0 && x >= 0 && y >= 0

  private lazy val seed = src.getLines().next().toInt

  private val dirs = Seq(Pos2D(1, 0), Pos2D(-1, 0), Pos2D(0, 1), Pos2D(0, -1))

  def partOne(): String =
    val start = Pos2D(1, 1)
    val goal = Pos2D(31, 39)

    def bfs(q: Queue[(Int, Pos2D)], visited: HashSet[Pos2D]): Int =
      val (step, pos) = q.dequeue()

      if pos == goal then step
      else
        dirs
          .map(_ + pos)
          .foreach(nextPos =>
            if !visited.contains(nextPos) && nextPos.isOpen() then
              visited.add(nextPos)
              q.enqueue((step + 1, nextPos))
          )
        bfs(q, visited)

    "%d".format(bfs(Queue((0, start)), HashSet(start)))

  def partTwo(): String =
    val start = Pos2D(1, 1)
    val limit = 50
    val q = Queue((0, start))
    val visited = HashSet(start)

    while q.size > 0 do
      val (step, pos) = q.dequeue()
      dirs
        .map(_ + pos)
        .foreach(nextPos =>
          if step < limit && !visited.contains(nextPos) && nextPos.isOpen() then
            visited.add(nextPos)
            q.enqueue((step + 1, nextPos))
        )

    "%d".format(visited.size)

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
