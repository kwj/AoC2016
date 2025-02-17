package net.vlax.aoc2016.sol

import java.security.MessageDigest
import scala.collection.immutable.Queue
import scala.io.BufferedSource

class Day17(src: BufferedSource) extends Solution:
  private inline val Width = 4 // X-axis
  private inline val Height = 4 // Y-axis

  private val seed = src.getLines().next()
  private val md5 = MessageDigest.getInstance("MD5")

  private case class Pos(x: Int, y: Int, path: String):
    enum Dirs(d: Int):
      case Up extends Dirs(0)
      case Down extends Dirs(1)
      case Left extends Dirs(2)
      case Right extends Dirs(3)

    val dirs = Seq(Dirs.Up, Dirs.Down, Dirs.Left, Dirs.Right)
    def isValid: Boolean = (1 to Width).contains(x) && (1 to Height).contains(y)
    def isGoal: Boolean = x == Width && y == Height

    def nextPos(dir: Dirs): Pos =
      dir match
        case Dirs.Up => Pos(x, y - 1, path + "U")
        case Dirs.Down => Pos(x, y + 1, path + "D")
        case Dirs.Left => Pos(x - 1, y, path + "L")
        case Dirs.Right => Pos(x + 1, y, path + "R")

    def nextPositions: Seq[Pos] =
      val arr = md5.digest((seed + path).getBytes())
      dirs
        .filter(idx => ((arr(idx.ordinal >> 1) >> (((idx.ordinal & 0b1) ^ 1) << 2)) & 0x0f) > 0x0a)
        .map(nextPos)
        .filter(_.isValid)

  private def getPaths(q: Queue[Pos]): LazyList[String] =
    q.dequeueOption match
      case Some((pos, nextQueue)) =>
        if pos.isGoal then pos.path #:: getPaths(nextQueue)
        else getPaths(nextQueue.appendedAll(pos.nextPositions))
      case None => LazyList.empty

  def partOne(): String =
    getPaths(Queue(Pos(1, 1, ""))).head

  def partTwo(): String =
    "%d".format(getPaths(Queue(Pos(1, 1, ""))).last.length())

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
