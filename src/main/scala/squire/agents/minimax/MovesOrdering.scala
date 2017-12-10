package squire.agents.minimax

import squire.base.State

trait MovesOrdering[S <: State[S]] { self: AlphaBetaNegamaxAgent[S] =>

  override def possibleMoves(state: S): Seq[(S#Move, S)] = {
    val moves = state.possibleMoves
    val zipped = moves.zip(moves.map(state.apply))
    counters.evaluatedHeuristics += moves.length
    zipped.sortBy(t => heuristic(t._2))
  }

}
