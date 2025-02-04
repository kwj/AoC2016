package net.vlax.aoc2016.sol

import scala.collection.mutable.HashMap
import scala.io.BufferedSource

class Day12(src: BufferedSource) extends Solution:
  private type Registers = HashMap[String, Int]
  private type Op = (Int, Registers) => (Int, Registers)

  object OpCode:
    def opCpy(opd1: String, opd2: String): Op =
      (ip: Int, reg: Registers) =>
        opd1.toIntOption match
          case Some(v) => reg(opd2) = v
          case None => reg(opd2) = reg(opd1)
        (ip + 1, reg)

    def opInc(opd1: String): Op =
      (ip: Int, reg: Registers) =>
        reg(opd1) = reg(opd1) + 1
        (ip + 1, reg)

    def opDec(opd1: String): Op =
      (ip: Int, reg: Registers) =>
        reg(opd1) = reg(opd1) - 1
        (ip + 1, reg)

    def opJnz(opd1: String, opd2: String): Op =
      (ip: Int, reg: Registers) =>
        opd1.toIntOption.getOrElse(reg(opd1)) match
          case v if v != 0 =>
            (ip + opd2.toInt, reg)
          case _ =>
            (ip + 1, reg)

  private def parseInput(src: BufferedSource): Array[Op] =
    import scala.util.matching.Regex

    val reInst = raw"^(\w+) (\w+) ?(.+)?".r

    src
      .getLines()
      .flatMap(_.split(", "))
      .map({
        case reInst(opc, opd1, opd2) =>
          opc match
            case "cpy" => OpCode.opCpy(opd1, opd2)
            case "inc" => OpCode.opInc(opd1)
            case "dec" => OpCode.opDec(opd1)
            case "jnz" => OpCode.opJnz(opd1, opd2)
        case line => throw new RuntimeException(s"Invalid instruction: $line")
      })
      .toArray

  private lazy val instructions = parseInput(src)

  private def execute(initReg: Registers): Int =
    val ipRange = (0 until instructions.size)

    def loop(ip: Int, reg: Registers): Int =
      if ipRange.contains(ip) then
        val (nextIp, nextReg) = instructions(ip)(ip, reg)
        loop(nextIp, nextReg)
      else reg("a")

    loop(0, initReg)

  def partOne(): String =
    val reg: Registers = HashMap("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0)

    "%d".format(execute(reg))

  def partTwo(): String =
    val reg: Registers = HashMap("a" -> 0, "b" -> 0, "c" -> 1, "d" -> 0)

    "%d".format(execute(reg))

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
