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
    def list: List[Int]

  private class Output(val id: Int) extends Target:
    private val box = HashSet[Int]()
    def post(v: Int) =
      box.add(v)
      None
    def list = box.toList

  private class Bot(val id: Int, nextTargets: List[Int]) extends Target:
    private val box = ArrayBuffer[Int]()
    def post(v: Int) =
      if box.length == 0 then
        box.addOne(v)
        None
      else
        val elems = List(box.remove(0), v).sorted
        Some(id, (elems(0), elems(1)), nextTargets.zip(elems).map((dst, v) => Delivery(dst, v)))
    def list = box.toList

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

  private lazy val (initState, instructions) = parseInput(src)

  private def execute(state: HashMap[Int, Target], order: Queue[Delivery]) = //: Unit =
    val stateMachine = state.clone()
    val log = ArrayBuffer[((Int, Int), Int)]()

    while !order.isEmpty do
      val Delivery(dst, v) = order.dequeue()
      stateMachine.get(dst).get.post(v) match
        case Some((id, tpl, nexts)) =>
          log.addOne((tpl, id))
          order.enqueueAll(nexts)
          ()
        case None => ()

    (stateMachine, log)

  private lazy val (finalState, log) = execute(initState, instructions)

  def partOne(): String =
    log.find((tpl, _) => tpl == (17, 61)) match
      case Some((_, id)) => "%d".format(id)
      case None => throw new RuntimeException("There is no answer.")

  def partTwo(): String =
    "%d".format(
      (0 to 2)
        .map(n => finalState.get(toTargetId("output", n)).get.list.product)
        .product
    )

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
