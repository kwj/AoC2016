package net.vlax.aoc2016.sol

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Queue}
import scala.io.BufferedSource

/*
  target:
    bot ID: 0 ..
    output ID: 0, 1 ..  --> -1, -2, ...

  instruction:
    Delivery(destination(target ID), value(microchip number))

  state machine:
    HashMap[target id, target]

  The `target.post(value)` method returns a Option value.
    When a bot receives a second microchip:
      Some(bot id, (value1, value2), list of next instructions)
      (Note: The bot returns all microchips, so it becomes to have no microchips)
    Otherwise:
      None
*/

class Day10(src: BufferedSource) extends Solution:
  private case class Delivery(dst: Int, v: Int)

  private trait Target:
    def post(v: Int): Option[(Int, (Int, Int), List[Delivery])]
    def list(): List[Int]

  private class Output(val id: Int) extends Target:
    private val box = HashSet[Int]()
    def post(v: Int) =
      box.add(v)
      None
    def list() = box.toList

  private class Bot(val id: Int, nextTargets: List[Int]) extends Target:
    private val box = ArrayBuffer[Int]()
    def post(v: Int) =
      if box.length == 0 then
        box.addOne(v)
        None
      else
        val elems = List(box.remove(0), v).sorted
        Some(id, (elems(0), elems(1)), nextTargets.zip(elems).map((dst, v) => Delivery(dst, v)))
    def list() = box.toList

  private def toTargetId(dst: String, n: Int) = if dst == "output" then -(n + 1) else n

  private def parseInput(src: BufferedSource): (HashMap[Int, Target], Queue[Delivery]) =
    import scala.util.matching.Regex

    val reBot = raw"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)".r
    val reValue = raw"value (\d+) goes to bot (\d+)".r

    val stateMachine = HashMap[Int, Target]()
    val instructions = Queue[Delivery]()

    src
      .getLines()
      .foreach({
        case reValue(v, dst) =>
          instructions.enqueue(Delivery(dst.toInt, v.toInt))
        case reBot(id, lowDst, low, highDst, high) =>
          val botId = id.toInt
          val lowId = toTargetId(lowDst, low.toInt)
          val highId = toTargetId(highDst, high.toInt)

          stateMachine.addOne((botId, Bot(botId, List(lowId, highId))))
          Seq(lowId, highId)
            .filter(_ < 0)
            .foreach(outputId => stateMachine.addOne(outputId, Output(outputId)))
        case line =>
          throw new RuntimeException(s"Invalid input data: $line")
      })

    (stateMachine, instructions)

  private lazy val (stateMachine, instructions) = parseInput(src)

  private val log = ArrayBuffer[((Int, Int), Int)]()

  private def execute(): Unit =
    while instructions.length > 0 do
      val Delivery(dst, v) = instructions.dequeue()
      stateMachine.get(dst).get.post(v) match
        case Some((id, tpl, nexts)) =>
          log.addOne((tpl, id))
          nexts.foreach(instructions.enqueue)
        case None => ()

  def partOne(): String =
    execute()
    log.find((tpl, _) => tpl == (17, 61)) match
      case Some((_, id)) => "%d".format(id)
      case None => throw new RuntimeException("There is no answer.")

  def partTwo(): String =
    execute()
    val ans =
      (0 to 2)
        .map(n => stateMachine.get(toTargetId("output", n)).get.list())
        .map(_.fold(1)(_ * _))
        .fold(1)(_ * _)

    "%d".format(ans)

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
