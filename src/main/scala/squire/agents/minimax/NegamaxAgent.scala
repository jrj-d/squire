package squire.agents.minimax

import java.text.DecimalFormat

import com.typesafe.scalalogging.Logger
import squire.base.{Finished, Playing, State}
import squire.base.Agent

class NegamaxAgent[S <: State[S]](heuristic: S => Double, maxDepth: Int) extends Agent[S] {

  val logger = Logger("NegamaxAgent")
  val formatter = new DecimalFormat("#.##")

  def play(state: S): S#Move = findBestMove(state, maxDepth)

  def findBestMove(state: S, depth: Int): S#Move = {

    val startTime = System.currentTimeMillis

    val bestMove = state.possibleMoves
        .map { m => (m, negamax(state.apply(m), depth - 1).invert) }
        .maxBy(_._2)
        ._1

    val duration = (System.currentTimeMillis - startTime) / 60000.0

    logger.info(s"$depth-ply depth evaluated in ${formatter.format(duration)} minutes. Best move: $bestMove")

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
