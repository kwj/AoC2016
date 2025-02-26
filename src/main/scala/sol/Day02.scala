package net.vlax.aoc2016.sol

import scala.io.BufferedSource

class Day02(src: BufferedSource) extends Solution:
  private class Pad(var x: Int, var y: Int, val keypad: Array[Array[Char]]):
    def move(dir: Char): Unit =
      dir match
        case 'U' if keypad(x - 1)(y) != '.' => x = x - 1
        case 'D' if keypad(x + 1)(y) != '.' => x = x + 1
        case 'L' if keypad(x)(y - 1) != '.' => y = y - 1
        case 'R' if keypad(x)(y + 1) != '.' => y = y + 1
        case _ => ()

    def getChar: Char = keypad(x)(y)

  private def parseInput(src: BufferedSource): Seq[Array[Char]] =
    src
      .getLines()
      .map(_.toArray)
      .toSeq

  private lazy val instructions = parseInput(src)

  private def getCode(keypad: Pad, insts: Seq[Array[Char]]): String =
    insts
      .map(directions =>
        directions.foreach(keypad.move)
        keypad.getChar
      )
      .mkString

  def partOne(): String =
    val layout: Array[Array[Char]] = Array(
      Array('.', '.', '.', '.', '.'),
      Array('.', '1', '2', '3', '.'),
      Array('.', '4', '5', '6', '.'),
      Array('.', '7', '8', '9', '.'),
      Array('.', '.', '.', '.', '.')
    )

    // layout(2)(2) is '5'
    val keypad = Pad(2, 2, layout)

    getCode(keypad, instructions)

  def partTwo(): String =
    val layout: Array[Array[Char]] = Array(
      Array('.', '.', '.', '.', '.', '.', '.'),
      Array('.', '.', '.', '1', '.', '.', '.'),
      Array('.', '.', '2', '3', '4', '.', '.'),
      Array('.', '5', '6', '7', '8', '9', '.'),
      Array('.', '.', 'A', 'B', 'C', '.', '.'),
      Array('.', '.', '.', 'D', '.', '.', '.'),
      Array('.', '.', '.', '.', '.', '.', '.')
    )

    // layout(3)(1) is '5'
    val keypad = Pad(3, 1, layout)

    getCode(keypad, instructions)

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
