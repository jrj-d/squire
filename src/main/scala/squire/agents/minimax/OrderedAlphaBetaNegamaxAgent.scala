package squire.agents.minimax

import com.typesafe.scalalogging.LazyLogging
import squire.base.{Agent, Finished, Playing, State}
import squire.utils.time

// scalastyle:off method.length

class OrderedAlphaBetaNegamaxAgent[S <: State[S]](heuristic: S => Double, maxDepth: Int) extends AlphaBetaNegamaxAgent(heuristic, maxDepth) {

  override def possibleMoves(state: S): Seq[(S#Move, S)] = {
    val moves = state.possibleMoves
    val zipped = moves.zip(moves.map(state.apply))
    zipped.sortBy(t => heuristic(t._2))
  }

}
