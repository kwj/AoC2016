package net.vlax.aoc2016.sol

import scala.io.{Source, BufferedSource}

trait Solution:
  def solve(): Unit
  def partOne(): String
  def partTwo(): String

object Solver:
  private def create(x: Int, input: BufferedSource): Option[Solution] =
    x match
      case 1 => Some(Day01(input))
      case 2 => Some(Day02(input))
      case 3 => Some(Day03(input))
      case 4 => Some(Day04(input))
      case 5 => Some(Day05(input))
      case 6 => Some(Day06(input))
      case 7 => Some(Day07(input))
      case 8 => Some(Day08(input))
      case 9 => Some(Day09(input))
      case 10 => Some(Day10(input))
      case 11 => Some(Day11(input))
      case 12 => Some(Day12(input))
      case 13 => Some(Day13(input))
      case 14 => Some(Day14(input))
      case 15 => Some(Day15(input))
      case 16 => Some(Day16(input))
      case 17 => Some(Day17(input))
      case 18 => Some(Day18(input))
      case 19 => Some(Day19(input))
      case _ =>
        println(f"There is no solver for Day $x%d")
        None

  def getSolver(line: String): Option[Solution] =
    import scala.util.matching.Regex

    val re1 = raw"(\d+)\s+(.+)".r.unanchored
    val re2 = raw"(\d+)".r.unanchored

    try
      line match
        case re1(number, fname) =>
          val n = number.toInt
          val input = Source.fromResource(f"$n%02d/$fname")
          create(n, input)
        case re2(number) =>
          val n = number.toInt
          val input = Source.fromResource(f"$n%02d/input")
          create(n, input)
        case _ =>
          println("Invalid input")
          None
    catch
      case e: Exception =>
        println(e)
        None
