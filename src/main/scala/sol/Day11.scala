package net.vlax.aoc2016.sol

import scala.collection.mutable.{HashMap, HashSet, Queue}
import scala.io.BufferedSource

class Day11(src: BufferedSource) extends Solution:
  private case class State(elev: Int, chipLocs: Vector[Int], genLocs: Vector[Int])
  private inline val firstFloor = 0
  private inline val fourthFloor = 3

  private def parseInput(src: BufferedSource): (Vector[Int], Vector[Int]) =
    import scala.util.matching.Regex

    val reItem = raw"a (\w+)(?:-compatible)? (\w+)?".r
    val chipMap = HashMap[String, Int]()
    val genMap = HashMap[String, Int]()

    src
      .getLines()
      .zipWithIndex
      .foreach((line, idx) =>
        reItem
          .findAllMatchIn(line)
          .foreach(m =>
            m.group(2) match
              case "microchip" => chipMap.addOne((m.group(1), idx))
              case _ => genMap.addOne((m.group(1), idx))
          )
      )

    val chips = (for name <- chipMap.keys.toSeq yield chipMap(name)).toVector
    val gens = (for name <- chipMap.keys.toSeq yield genMap(name)).toVector

    (chips, gens)

  private lazy val (initChips, initGens) = parseInput(src)

  private def stateHash(state: State): Long =
    val State(floor, chips, gens) = state
    chips
      .zip(gens)
      .map((c, g) => (c << 2) + g)
      .sorted
      .foldLeft(floor.toLong)((acc, x) => (acc << 4) + x)

  private inline def selectItems(locs: Vector[Int], floor: Int): Vector[Int] =
    locs.zipWithIndex.filter((loc, idx) => loc == floor).map(_(1))

  private def floorItems(state: State): (Vector[Int], Vector[Int], Vector[Int]) =
    val State(floor, chipLocs, genLocs) = state

    val chips = selectItems(chipLocs, floor)
    val gens = selectItems(genLocs, floor)
    val localPairs = chips.filter(gens.contains)

    (chips, gens, localPairs)

  private def updateLocs(locs: Vector[Int], movs: Vector[Int], dir: Int): Vector[Int] =
    movs.foldLeft(locs)((vs, idx) => vs.updated(idx, vs(idx) + dir))

  private def moveChips(
      state: State,
      upDown: Vector[Int],
      chips: Vector[Int]
  ): Iterator[(State, Vector[Int])] =
    for
      movs <- chips.combinations(2) ++ chips.combinations(1)
      dir <- upDown
      newFloor = state.elev + dir
    yield (State(newFloor, updateLocs(state.chipLocs, movs, dir), state.genLocs), movs)

  private def moveGens(
      state: State,
      upDown: Vector[Int],
      gens: Vector[Int]
  ): Iterator[(State, Vector[Int])] =
    for
      movs <- gens.combinations(2) ++ gens.combinations(1)
      dir <- upDown
      newFloor = state.elev + dir
      newGenLocs = updateLocs(state.genLocs, movs, dir)
      srcChips = selectItems(state.chipLocs, state.elev)
      dstChips = selectItems(state.chipLocs, newFloor)
    yield (State(newFloor, state.chipLocs, newGenLocs), srcChips ++ dstChips)

  private def movePair(
      state: State,
      upDown: Vector[Int],
      pairs: Vector[Int]
  ): Iterator[(State, Vector[Int])] =
    if pairs.isEmpty then Iterator.empty
    else
      for
        dir <- upDown.iterator
        newFloor = state.elev + dir
        newChipLocs = updateLocs(state.chipLocs, Vector(pairs(0)), dir)
        newGenLocs = updateLocs(state.genLocs, Vector(pairs(0)), dir)
        dstChips = selectItems(newChipLocs, newFloor)
      yield (State(newFloor, newChipLocs, newGenLocs), dstChips)

  private def nextStates(state: State): Iterator[(State, Vector[Int])] =
    val State(floor, chipLocs, genLocs) = state
    val (chips, gens, localPairs) = floorItems(state)
    val dirs = Vector(1, -1).filter(x => (firstFloor to fourthFloor).contains(x + state.elev))

    moveChips(state, dirs, chips)
      ++ moveGens(state, dirs, gens)
      ++ movePair(state, dirs, localPairs)

  private def execute(initState: State): Int =
    def isComplete(state: State): Boolean =
      state.chipLocs.forall(_ == fourthFloor) && state.genLocs.forall(_ == fourthFloor)

    def isValidState(state: State, checkChips: Vector[Int]): Boolean =
      checkChips.forall(c =>
        val floor = state.chipLocs(c)
        state.genLocs(c) == floor || !state.genLocs.contains(floor)
      )

    def bfs(q: Queue[(Int, State)], seen: HashSet[Long]): Int =
      val (step, state) = q.dequeue()

      if isComplete(state) then step
      else
        nextStates(state)
          .filter(isValidState)
          .foreach((st, _) =>
            val hash = stateHash(st)
            if !seen.contains(hash) then
              seen.add(hash)
              q.enqueue((step + 1, st))
          )
        bfs(q, seen)

    bfs(Queue((0, initState)), HashSet(stateHash(initState)))

  def partOne(): String =
    val ans = execute(State(firstFloor, initChips, initGens))
    "%d".format(ans)

  def partTwo(): String =
    val ans = execute(
      State(
        firstFloor,
        initChips.appendedAll(Vector(firstFloor, firstFloor)),
        initGens.appendedAll(Vector(firstFloor, firstFloor))
      )
    )
    "%d".format(ans)

  def solve(): Unit =
    printf("%s\n", partOne())
    printf("%s\n", partTwo())
