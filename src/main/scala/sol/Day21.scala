package net.vlax.aoc2016.sol

import scala.collection.immutable.ArraySeq
import scala.io.BufferedSource

class Day21(src: BufferedSource) extends Solution:
  private enum Op:
    case SwapPos(x: Int, y: Int)
    case SwapLetter(x: Char, y: Char)
    case RotateR(x: Int)
    case RotateL(x: Int)
    case RotateBasedR(x: Char)
    case RotateBasedL(x: Char)
    case Reverse(x: Int, y: Int)
    case Move(x: Int, y: Int)

  private def parseInput(src: BufferedSource): Seq[Op] =
    import scala.util.matching.Regex

    val reSwap = raw"swap (\w+) (\w+).* (\w+)".r
    val reRotate = raw"rotate (\w+) (\w+).* (\w+)".r
    val reReverse = raw"reverse \w+ (\w+) \w+ (\w+)".r
    val reMove = raw"move \w+ (\w+).* (\w+)".r

    src
      .getLines()
      .map({
        case reSwap(kind, x, y) =>
          if kind == "position" then Op.SwapPos(x.toInt, y.toInt) else Op.SwapLetter(x(0), y(0))
        case reRotate(kind, x, y) =>
          kind match
            case "left" => Op.RotateL(x.toInt)
            case "right" => Op.RotateR(x.toInt)
            case "based" => Op.RotateBasedR(y(0))
            case _ => throw new RuntimeException(s"Invalid rotation: $kind")
        case reReverse(x, y) => Op.Reverse(x.toInt, y.toInt)
        case reMove(x, y) => Op.Move(x.toInt, y.toInt)
        case line => throw new RuntimeException(s"Invalid operation: $line")
      })
      .toSeq

  private lazy val operations = parseInput(src)

  private def opSwap(arr: ArraySeq[Char], x: Int, y: Int): ArraySeq[Char] =
    arr.updated(x, arr(y)).updated(y, arr(x))

  private def opSwapLetter(arr: ArraySeq[Char], x: Char, y: Char): ArraySeq[Char] =
    arr.map({
      case ch if ch == x => y
      case ch if ch == y => x
      case ch => ch
    })

  private def opRotateR(arr: ArraySeq[Char], x: Int): ArraySeq[Char] =
    arr.takeRight(x) ++ arr.dropRight(x)

  private def opRotateL(arr: ArraySeq[Char], x: Int): ArraySeq[Char] =
    arr.drop(x) ++ arr.take(x)

  private def opRotateBasedR(arr: ArraySeq[Char], x: Char): ArraySeq[Char] =
    val idx = arr.indexOf(x)
    val cnt = (idx + (if idx >= 4 then 2 else 1)) % arr.length

    opRotateR(arr, cnt)

  private def opRotateBasedL(arr: ArraySeq[Char], x: Char): ArraySeq[Char] =
    val len = arr.length
    val idx = arr.indexOf(x)

    var origIdx = len - 1
    while origIdx >= 0 do
      val cnt = (origIdx + (if origIdx >= 4 then 2 else 1)) % len
      if (origIdx + cnt) % len == idx then return opRotateL(arr, cnt)
      else origIdx -= 1

    throw new RuntimeException("unreachable/opRotateBasedL")

  private def opReverse(arr: ArraySeq[Char], x: Int, y: Int): ArraySeq[Char] =
    val pre = arr.take(x)
    val body = arr.slice(x, y + 1)
    val post = arr.drop(y + 1)

    pre ++ body.reverse ++ post

  private def opMove(arr: ArraySeq[Char], x: Int, y: Int): ArraySeq[Char] =
    val removedArr = arr.take(x) ++ arr.drop(x + 1)
    val (hd, tl) = removedArr.splitAt(y)

    (hd :+ arr(x)) ++ tl

  private def exec(arr: ArraySeq[Char], op: Op): ArraySeq[Char] =
    op match
      case Op.SwapPos(x, y) => opSwap(arr, x, y)
      case Op.SwapLetter(x, y) => opSwapLetter(arr, x, y)
      case Op.RotateR(x) => opRotateR(arr, x)
      case Op.RotateL(x) => opRotateL(arr, x)
      case Op.RotateBasedR(x) => opRotateBasedR(arr, x)
      case Op.RotateBasedL(x) => opRotateBasedL(arr, x)
      case Op.Reverse(x, y) => opReverse(arr, x, y)
      case Op.Move(x, y) => opMove(arr, x, y)

  private def unscramble(op: Op): Op =
    op match
      case Op.RotateR(x) => Op.RotateL(x)
      case Op.RotateL(x) => Op.RotateR(x)
      case Op.RotateBasedR(x) => Op.RotateBasedL(x)
      case Op.Move(x, y) => Op.Move(y, x)
      case _ => op

  def partOne(): String =
    val plainPasswd = ArraySeq.from("abcdefgh")

    operations
      .foldLeft(plainPasswd)(exec)
      .mkString

  def partTwo(): String =
    val scrambledPasswd = ArraySeq.from("fbgdceah")

    operations.reverse
      .map(unscramble)
      .foldLeft(scrambledPasswd)(exec)
      .mkString

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
