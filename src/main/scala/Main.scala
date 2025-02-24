package net.vlax.aoc2016

@main def repl(): Unit =
  import scala.io.StdIn
  import net.vlax.aoc2016.sol.Solver

  println("Please enter a date number, or Ctrl-D to exit.")
  Iterator
    .continually(Option(StdIn.readLine("> ")))
    .takeWhile(_.nonEmpty)
    .map(lineOpt => Solver.getSolver(lineOpt.get))
    .filter(_.nonEmpty)
    .foreach(solver =>
      val startTime = System.currentTimeMillis
      solver.get.solve()
      val endTime = System.currentTimeMillis
      println("Elapsed time: " + (endTime - startTime) + " msec.\n")
    )
