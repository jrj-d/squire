package squire.agents.minimax

import squire.utils.Tabulator

class Counters {
  var traversedNodes = 0L
  var evaluatedHeuristics = 0L
  var betaCutoffs = 0L
  var finalNodes = 0L
  var leafNodes = 0L
  var traversedNodesInQuiescence = 0L
  var betaCutoffsInQuiescence = 0L

  def print(duration: Double): String = {
    val table = Seq(
      Seq("Metrics", "Value"),
      Seq("duration", duration),
      Seq("nodes per second", 1e3 * (traversedNodes + traversedNodesInQuiescence) / duration),
      Seq("traversed nodes", traversedNodes),
      Seq("evaluated heuristics", evaluatedHeuristics),
      Seq("beta cutoffs", betaCutoffs),
      Seq("final nodes", finalNodes),
      Seq("leaf nodes", leafNodes),
      Seq("traversed nodes in quiescence", traversedNodesInQuiescence),
      Seq("beta cutoffs in quiescence", betaCutoffsInQuiescence)
    )
    Tabulator.format(table)
  }

  def reset(): Unit = {
    traversedNodes = 0L
    evaluatedHeuristics = 0L
    betaCutoffs = 0L
    finalNodes = 0L
    leafNodes = 0L
    traversedNodesInQuiescence = 0L
    betaCutoffsInQuiescence = 0L
  }
}
