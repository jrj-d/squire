package squire.agents.minimax

import com.typesafe.scalalogging.LazyLogging
import squire.base.{Agent, Finished, Playing, State}
import squire.utils.time

// scalastyle:off method.length

class AlphaBetaNegamaxAgent[S <: State[S]](heuristic: S => Double, maxDepth: Int) extends Agent[S] with LazyLogging {

  def play(state: S): S#Move = findBestMove(state, maxDepth)

  def findBestMove(state: S, depth: Int): S#Move = {

    val counters = new Counters

    def negamax(state: S, depth: Int, alpha: Score, beta: Score): Score = {
      counters.traversedNodes += 1
      logger.debug(s"Remaining depth is $depth, evaluating\n$state")
      state.evaluate match {
        case Finished(result) => {
          counters.finalNodes += 1
          logger.debug(s"State is final: $result")
          Result(result)
        }
        case Playing =>
          if(depth == 0) {
            val value = heuristic(state)
            counters.evaluatedHeuristics += 1
            logger.debug(s"Heuristic evaluation: $value")
            Heuristic(value)
          } else {
            var bestValue: Score = Result(Double.MinValue)
            var currentAlpha = alpha
            var remainingMoves = state.possibleMoves
            while(remainingMoves.nonEmpty && currentAlpha < beta) {
              val move = remainingMoves.head
              remainingMoves = remainingMoves.tail
              val currentValue = negamax(state.apply(move), depth - 1, beta.invert, currentAlpha.invert).invert
              bestValue = if(bestValue > currentValue) bestValue else currentValue
              currentAlpha = if(currentAlpha > currentValue) currentAlpha else currentValue
            }
            if(remainingMoves.nonEmpty) counters.betaCutoffs += 1
            logger.debug(s"Alpha-beta pruned minimax evaluation: $bestValue")
            bestValue
          }
      }
    }

    val (bestMove, duration) = time {
      val moves = state.possibleMoves
      var bestMove = moves.head
      var bestValue: Score = Result(Double.MinValue)
      var currentAlpha: Score = Result(Double.MinValue)
      for(move <- moves) {
        val currentValue = negamax(state.apply(move), depth - 1, Result(Double.MinValue), currentAlpha.invert).invert
        if(currentValue > bestValue) {
          bestValue = currentValue
          bestMove = move
        }
        currentAlpha = if(currentAlpha > currentValue) currentAlpha else currentValue
      }
      bestMove
    }

    logger.info(f"Best move at $depth-ply depth is $bestMove")
    logger.info(s"\n${counters.print(duration)}")

    bestMove
  }

}
