package squire.agents.minimax

import squire.utils.Tabulator

class Counters {
  var traversedNodes = 0L
  var evaluatedHeuristics = 0L
  var betaCutoffs = 0L
  var finalNodes = 0L

  def print(duration: Double): String = {
    val table = Seq(
      Seq("Metrics", "Value"),
      Seq("duration", duration),
      Seq("nodes per second", 1e3 * traversedNodes / duration),
      Seq("traversed nodes", traversedNodes),
      Seq("evaluated heuristics", evaluatedHeuristics),
      Seq("beta cutoffs", betaCutoffs),
      Seq("final nodes", finalNodes),
    )
    Tabulator.format(table)
  }
}
