package net.vlax.aoc2016.sol

/*
# The following is a Python implementation of the equivalent of the given program.
# It is much faster than the original program because it uses addition and multiplication.

part1 = False

# factorial (7! or 12!)
a = 7 if part1 == True else 12
for k in range(1, a):
    a *= k

# add a constant
c = 86
d = 77
a += c * d

print(a)

----------------------
[Part 2]
Since the `tgl` instruction changes other instructions at running,
it is normally impossible to optimize before program execution.

After analyzing my given input data, the factorial computation in the first half
of the program was outside the scope of changing by the `tgl` instruction,
I therefore decided to optimize only this part of the program.
 */

import scala.collection.mutable.HashMap
import scala.io.BufferedSource

class Day23(src: BufferedSource) extends Solution:
  private type State = HashMap[String, Int]

  private enum Operand:
    case Reg(x: String)
    case Num(x: Int)

  private enum Mnemonic:
    case Cpy(x: Operand, y: Operand)
    case Inc(x: Operand)
    case Dec(x: Operand)
    case Jnz(x: Operand, y: Operand)
    case Tgl(x: Operand)
    case Mul(x: Operand, y: Operand, z: Operand)
    case Nop

  private def parseInput(src: BufferedSource): Array[Mnemonic] =
    import scala.util.matching.Regex

    val reInst = raw"^(\w+) ([\w\-]+) ?(.+)?".r

    src
      .getLines()
      .flatMap(_.split(", "))
      .map({
        case reInst(opc, opd1, opd2) =>
          opc match
            case "cpy" =>
              val x = opd1.toIntOption match
                case Some(n) => Operand.Num(n)
                case None => Operand.Reg(opd1)
              val y = Operand.Reg(opd2)
              Mnemonic.Cpy(x, y)
            case "inc" =>
              Mnemonic.Inc(Operand.Reg(opd1))
            case "dec" =>
              Mnemonic.Dec(Operand.Reg(opd1))
            case "jnz" =>
              val x = opd1.toIntOption match
                case Some(n) => Operand.Num(n)
                case None => Operand.Reg(opd1)
              val y = opd2.toIntOption match
                case Some(n) => Operand.Num(n)
                case None => Operand.Reg(opd2)
              Mnemonic.Jnz(x, y)
            case "tgl" =>
              val x = opd1.toIntOption match
                case Some(n) => Operand.Num(n)
                case None => Operand.Reg(opd1)
              Mnemonic.Tgl(x)
        case line => throw new RuntimeException(s"Invalid instruction: $line")
      })
      .toArray

  private lazy val program = parseInput(src)

  private def getValue(st: State, op: Operand): Int =
    op match
      case Operand.Num(n) => n
      case Operand.Reg(reg) => st(reg)

  private def opCpy(st: State, op1: Operand, op2: Operand): State =
    st("ip") += 1
    op2 match
      case Operand.Reg(reg) => st(reg) = getValue(st, op1)
      case Operand.Num(_) => ()

    st

  private def opInc(st: State, op: Operand): State =
    st("ip") += 1
    op match
      case Operand.Reg(reg) => st(reg) += 1
      case Operand.Num(_) => ()

    st

  private def opDec(st: State, op: Operand): State =
    st("ip") += 1
    op match
      case Operand.Reg(reg) => st(reg) -= 1
      case Operand.Num(_) => ()

    st

  private def opJnz(st: State, op1: Operand, op2: Operand): State =
    if getValue(st, op1) == 0 then st("ip") += 1
    else st("ip") += getValue(st, op2)

    st

  private def opTgl(st: State, op: Operand, instructions: Array[Mnemonic]): State =
    val addr = st("ip") + getValue(st, op)

    st("ip") += 1
    if instructions.indices.contains(addr) then
      instructions(addr) match
        case Mnemonic.Cpy(x, y) => instructions(addr) = Mnemonic.Jnz(x, y)
        case Mnemonic.Jnz(x, y) => instructions(addr) = Mnemonic.Cpy(x, y)
        case Mnemonic.Inc(x) => instructions(addr) = Mnemonic.Dec(x)
        case Mnemonic.Dec(x) => instructions(addr) = Mnemonic.Inc(x)
        case Mnemonic.Tgl(x) => instructions(addr) = Mnemonic.Inc(x)
        case _ => ()

    st

  private def opMul(st: State, op1: Operand, op2: Operand, op3: Operand): State =
    st("ip") += 1
    op3 match
      case Operand.Reg(reg) => st(reg) = getValue(st, op1) * getValue(st, op2)
      case Operand.Num(_) => ()

    st

  private def opNop(st: State): State =
    st("ip") += 1

    st

  private def execute(initReg: State, instructions: Array[Mnemonic]): Int =
    val ipRange = instructions.indices

    def loop(st: State): Int =
      ipRange.contains(st("ip")) match
        case true =>
          val nextState = instructions(st("ip")) match
            case Mnemonic.Cpy(x, y) => opCpy(st, x, y)
            case Mnemonic.Inc(x) => opInc(st, x)
            case Mnemonic.Dec(x) => opDec(st, x)
            case Mnemonic.Jnz(x, y) => opJnz(st, x, y)
            case Mnemonic.Tgl(x) => opTgl(st, x, instructions)
            case Mnemonic.Mul(x, y, z) => opMul(st, x, y, z)
            case Mnemonic.Nop => opNop(st)
          loop(nextState)
        case false => st("a")

    loop(initReg)

  private def optimize(arr: Array[Mnemonic]): Array[Mnemonic] =
    for i <- 0 to arr.length - 6
    do
      (arr(i), arr(i + 1), arr(i + 2), arr(i + 3), arr(i + 4), arr(i + 5)) match
        case (
              Mnemonic.Cpy(op1, op2),
              Mnemonic.Inc(op3),
              Mnemonic.Dec(op4),
              Mnemonic.Jnz(op5, op6),
              Mnemonic.Dec(op7),
              Mnemonic.Jnz(op8, op9)
            )
            if op2 == op4 && op4 == op5 && op6 == Operand.Num(-2) && op7 == op8 && op9 == Operand.Num(-5) =>
          arr(i) = Mnemonic.Mul(op1, op7, op3)
          arr(i + 1) = Mnemonic.Cpy(Operand.Num(0), op4)
          arr(i + 2) = Mnemonic.Cpy(Operand.Num(0), op7)
          arr(i + 3) = Mnemonic.Nop
          arr(i + 4) = Mnemonic.Nop
          arr(i + 5) = Mnemonic.Nop
        case _ => ()

    arr

  def partOne(): String =
    val state: State = HashMap("a" -> 7, "b" -> 0, "c" -> 0, "d" -> 0, "ip" -> 0)

    "%d".format(execute(state, program.clone()))

  def partTwo(): String =
    val state: State = HashMap("a" -> 12, "b" -> 0, "c" -> 0, "d" -> 0, "ip" -> 0)

    "%d".format(execute(state, optimize(program.clone())))

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
