package net.vlax.aoc2016

@main def repl(): Unit =
  import scala.collection.Iterator
  import scala.io.StdIn
  import net.vlax.aoc2016.sol.{Solution, Solver}

  println("Please enter a date number, or Ctrl-D to exit.")
  Iterator
    .continually(Option(StdIn.readLine("> ")))
    .takeWhile(_.nonEmpty)
    .map(lineOpt => Solver.getSolver(lineOpt.get))
    .filter(_.nonEmpty)
    .foreach(_.get.solve())
