package squire.agents.minimax

import com.typesafe.scalalogging.Logger
import squire.base.{Agent, Finished, Playing, State}
import squire.utils.time

class NegamaxAgent[S <: State[S]](heuristic: S => Double, maxDepth: Int) extends Agent[S] {

  val logger = Logger("NegamaxAgent")

  def play(state: S): S#Move = findBestMove(state, maxDepth)

  def findBestMove(state: S, depth: Int): S#Move = {

    val (bestMove, duration) = time(state.possibleMoves
        .map { m => (m, negamax(state.apply(m), depth - 1).invert) }
        .maxBy(_._2)
        ._1
    )

    logger.info(f"$depth-ply depth evaluated in $duration%1.0f ms. Best move: $bestMove")

    bestMove
  }

  def negamax(state: S, depth: Int): Score = {
    state.evaluate match {
      case Finished(result) => Result(result)
      case Playing =>
        if(depth == 0) {
          Heuristic(heuristic(state))
        } else {
          state.possibleMoves
            .map(state.apply)
            .map(negamax(_, depth - 1).invert)
            .max
        }
    }
  }

}
