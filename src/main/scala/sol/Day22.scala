package net.vlax.aoc2016.sol

/*
[Part 2]
There are three types of nodes in my given input data.

              size      used     Qty
  -------------------------------------
  type/1       91T       0T        1
  type/2     80T-99T   60T-79T   987
  type/3    500T-510T 490T-499T   32

It is clear that data movement must begin with the type/1 node which usage is 0 TB.
It's like a 15 puzzle. In addition, it is clear that type/3 nodes' data are not movable.

The input data is approximately as shown in the figure below:

..........G
...........
..#########
...........
...........
....._.....
...........

  G: goal data (type/2 node)
  _: empty node (type/1 node)
  .: type/2 node
  #: type/3 node


Assuming that no type/3 node exists at coordinates less than y=2,
the solving method is as follows:

Step 1: Move the empty node to the left of the goal data

    ........._G
    ...........
    ..#########
    ...........
    ...........
    ...........
    ...........

Step 2: Move the goal data to the top-left corner.

    G_.........
    ...........
    ..#########
    ...........
    ...........
    ...........
    ...........
 */

import scala.io.BufferedSource

class Day22(src: BufferedSource) extends Solution:
  private case class Node(x: Int, y: Int, size: Int, used: Int, avail: Int)
  private case class State(x: Int, y: Int, step: Int)

  private def stateOrder(state: State): Int = -state.step

  private def stateHash(state: State): (Int, Int) = (state.x, state.y)

  private def parseInput(src: BufferedSource) = // : Seq[(Int, Int)] =
    import scala.util.matching.Regex

    val reNode = raw"/dev.*-x(\d+)-y(\d+) +(\d+)T +(\d+)T +(\d+)T.*".r

    src
      .getLines()
      .collect({ case reNode(x, y, size, used, avail) =>
        Node(x.toInt, y.toInt, size.toInt, used.toInt, avail.toInt)
      })
      .toSeq

  private lazy val allNodes = parseInput(src)

  private def makeGrid(nodes: Seq[Node]): Array[Array[Node]] =
    val width = nodes.map(_.x).max + 1
    val height = nodes.map(_.y).max + 1

    val grid = Array.ofDim[Node](height, width)
    for n <- nodes do grid(n.y)(n.x) = n

    grid

  private def nextStates(st: State, grid: Array[Array[Node]]): Seq[State] =
    val State(x0, y0, step) = st

    Seq((x0 + 1, y0), (x0 - 1, y0), (x0, y0 + 1), (x0, y0 - 1))
      .filter((x, y) => grid.indices.contains(y) && grid(0).indices.contains(x))
      .filter((x, y) => grid(y)(x).used < grid(y0)(x0).size)
      .map((x, y) => State(x, y, step + 1))

  private def getStep1(grid: Array[Array[Node]]): Int =
    // Dijkstra
    import scala.collection.mutable.{HashSet, PriorityQueue}

    val startNode = allNodes.find(_.used == 0).get // empty node
    val start = State(startNode.x, startNode.y, 0)

    val target = (grid(0).indices.max - 1, 0) // top-right node

    val pq = PriorityQueue[State](start)(Ordering.by(stateOrder))
    val seen = HashSet(stateHash(start))

    while !pq.isEmpty do
      val st = pq.dequeue()
      if target == (st.x, st.y) then return st.step

      for nextState <- nextStates(st, grid)
      do
        val hash = stateHash(nextState)
        if !seen.contains(hash) then
          seen.add(hash)
          pq.enqueue(nextState)

    throw new RuntimeException(s"There is no answer.")

  private def getStep2(grid: Array[Array[Node]]): Int =
    val initX = grid(0).indices.max - 1

    1 + initX * 5

  def partOne(): String =
    val used = allNodes.collect({ case n if n.used > 0 => (n.used, (n.x, n.y)) }).sortBy(_(0))
    val avail = allNodes.map(n => (n.avail, (n.x, n.y))).sortBy(_(0))

    "%d".format(used.map(u => avail.dropWhile(a => a(0) < u(0)).count(a => u(1) != a(1))).sum)

  def partTwo(): String =
    val grid = makeGrid(allNodes)

    "%d".format(getStep1(grid) + getStep2(grid))

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
