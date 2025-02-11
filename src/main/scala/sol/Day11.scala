package net.vlax.aoc2016.sol

/*
Actually, I got the answer by comparing the number of microchips and generators
regardless of their type. However, I couldn't prove that this method was correct.
*/

// Note: this solution assumes that the fourth floor is the top floor.
//
// type(kind):      A  B  C
// ----------------------------
// microchip(chip): 0, 2, 4, ...
// generator(gen):  1, 3, 5, ...

import scala.io.BufferedSource

class Day11(src: BufferedSource) extends Solution:
  private case class State(elev: Int, locs: Vector[Vector[Int]], step: Int, priority: Int)
  private def stateOrder(state: State): (Int, Int) = (-state.priority, state.step)

  private inline def toGen(n: Int): Int = n | 0b1
  private inline def toChip(n: Int): Int = n & ~0b1
  private inline def isGen(n: Int): Boolean = (n & 0b1) == 1
  private inline def isChip(n: Int): Boolean = (n & 0b1) == 0

  private def parseInput(src: BufferedSource): Vector[Vector[Int]] =
    import scala.collection.mutable.HashMap
    import scala.util.matching.Regex

    val reNothing = raw"nothing".r.unanchored
    val reItem = raw"a (\w+)(?:-compatible)? (\w+)?".r
    val kindMap = HashMap[String, Int]()

    src
      .getLines()
      .zipWithIndex
      .map((line, idx) =>
        if reNothing.matches(line) then Vector()
        else
          val kinds =
            for
              m <- reItem.findAllMatchIn(line)
              kind = kindMap.getOrElseUpdate(m.group(1), kindMap.size) << 1
            yield m.group(2) match
              case "generator" => toGen(kind)
              case _ => kind
          kinds.toVector
      )
      .toVector

  private lazy val locations = parseInput(src)
  private var nKinds = 0

  private def stateHash(elev: Int, locs: Vector[Vector[Int]]): Long =
    assert(nKinds != 0)
    val arr = Array.fill(nKinds)(0L)
    locs.zipWithIndex.map((items, idx) =>
      items.foreach(x => arr(x >> 1) |= (idx << ((x & 0b1) << 1)))
    )
    arr.sorted.foldLeft(elev.toLong)((acc, x) => (acc << 4) | x)

  // heuristic function for A*
  // This function returns the minimum steps when there are no constraints
  private def h(elev: Int, locs: Vector[Vector[Int]]): Int =
    // number of steps required to move all items to up one floor.
    def m(x: Int) = if x < 2 then x else 2 * (x - 2) + 1

    val arr = locs.map(_.length).toArray
    val lowestFloor = arr.indexWhere(_ != 0)
    val gap = elev - lowestFloor
    if gap > 0 then
      arr(lowestFloor) += 1
      arr(elev) -= 1

    val (_, v) =
      arr.init
        .foldLeft((0, gap))((tpl, x) =>
          val nItems = tpl(0) + x
          (nItems, tpl(1) + m(nItems))
        )
    v

  private def selectItems(items: Vector[Int]): Iterator[Vector[Int]] =
    val (gens, chips) = items.partition(isGen)
    val pairIt =
      gens.find(g => chips.exists(_ == toChip(g))) match
        case Some(g) => Iterator.apply(Vector(g, toChip(g)))
        case None => Iterator.empty

    gens.combinations(2)
      ++ gens.combinations(1)
      ++ chips.combinations(2)
      ++ chips.combinations(1)
      ++ pairIt

  private def moveItems(
      locs: Vector[Vector[Int]],
      src: Int,
      dst: Int,
      items: Vector[Int]
  ): Vector[Vector[Int]] =
    val newSrc = locs(src).filterNot(x => items.exists(_ == x))
    val newDst = locs(dst) ++ items

    locs.updated(src, newSrc).updated(dst, newDst)

  private def isSafe(items: Vector[Int]): Boolean =
    val (gens, chips) = items.partition(isGen)
    chips.forall(c => gens.isEmpty || gens.contains(toGen(c)))

  private def isCompleted(locs: Vector[Vector[Int]]): Boolean = locs.init.forall(_.isEmpty)

  private def findMinTransition(start: State): Int =
    import scala.collection.mutable.{HashSet, PriorityQueue}

    val pq = PriorityQueue[State](start)(Ordering.by(stateOrder))
    val seen = HashSet(stateHash(start.elev, start.locs))

    while !pq.isEmpty do
      val State(floor, locs, step, _) = pq.dequeue()
      if isCompleted(locs) then return step

      val newStep = step + 1
      for
        items <- selectItems(locs(floor))
        newFloor <- Seq(floor + 1, floor - 1) if locs.indices.contains(newFloor)
        newLocs = moveItems(locs, floor, newFloor, items)
      do
        if isSafe(newLocs(floor)) && isSafe(newLocs(newFloor)) then
          val hash = stateHash(newFloor, newLocs)
          if !seen.contains(hash) then
            seen.add(hash)
            pq.enqueue(State(newFloor, newLocs, newStep, newStep + h(newFloor, newLocs)))

    -1 // FATAL ERROR: There is no answer

  def partOne(): String =
    // Must be set before findMinTransition() is called.
    nKinds = locations.map(_.length).sum >> 1

    "%d".format(findMinTransition(State(0, locations, 0, h(0, locations))))

  def partTwo(): String =
    inline val nAddItems = 4
    val startId = locations.map(_.length).sum
    val newFirstFloor = locations(0).appendedAll(startId until (startId + nAddItems))
    val newLocs = locations.updated(0, newFirstFloor)

    // Must be set before findMinTransition() is called.
    nKinds = (startId + nAddItems) >> 1

    "%d".format(findMinTransition(State(0, newLocs, 0, h(0, newLocs))))

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
