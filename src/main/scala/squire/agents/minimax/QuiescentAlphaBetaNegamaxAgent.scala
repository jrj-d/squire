package squire.agents.minimax

import squire.base.{Finished, Playing, State}
import squire.utils.time

class QuiescentAlphaBetaNegamaxAgent[S <: State[S]](heuristic: S => Double, isQuiet: (S, S#Move, S) => Boolean, maxDepth: Int)
      extends AlphaBetaNegamaxAgent(heuristic, maxDepth) {

  def quiescence(previousState: S, move: S#Move, state: S, alpha: Score, beta: Score): Score = {
    counters.traversedNodesInQuiescence += 1
    state.evaluate match {
      case Finished(result) =>
        counters.finalNodes += 1
        counters.leafNodes += 1
        logger.debug(s"State is final: $result")
        Result(result)
      case _ =>
        val standingPat = Heuristic(heuristic(state))
        counters.evaluatedHeuristics += 1
        if(standingPat >= beta) {
          return beta //todo should i return standing pat?
        }
        var bestValue: Score = Result(Double.MinValue)
        var currentAlpha = alpha
        if(currentAlpha < standingPat) {
          currentAlpha = standingPat
        }
        var remainingMoves = possibleMoves(state)
        while (remainingMoves.nonEmpty && currentAlpha < beta) {
          val (nextMove, nextState) = remainingMoves.head
          remainingMoves = remainingMoves.tail
          val currentValue = if(isQuiet(state, nextMove, nextState)) {
            evaluateStatically(nextState)
          } else {
            quiescence(state, nextMove, nextState, beta.invert, currentAlpha.invert).invert
          }
          bestValue = if (bestValue > currentValue) bestValue else currentValue
          currentAlpha = if (currentAlpha > currentValue) currentAlpha else currentValue
        }
        if (remainingMoves.nonEmpty) counters.betaCutoffsInQuiescence += 1
        logger.debug(s"Quiescence evaluation: $bestValue")
        bestValue
    }
  }

  def modifiedNegamax(previousState: S, move: S#Move, state: S, depth: Int, alpha: Score, beta: Score): Score = {
    logger.debug(s"Remaining depth is $depth, evaluating\n$state")
    if(depth == 0) {
      quiescence(previousState, move, state, alpha, beta)
    } else {
      counters.traversedNodes += 1
      state.evaluate match {
        case Finished(result) =>
          counters.finalNodes += 1
          counters.leafNodes += 1
          logger.debug(s"State is final: $result")
          Result(result)
        case Playing =>
          var bestValue: Score = Result(Double.MinValue)
          var currentAlpha = alpha
          var remainingMoves = possibleMoves(state)
          while (remainingMoves.nonEmpty && currentAlpha < beta) {
            val (nextMove, nextState) = remainingMoves.head
            remainingMoves = remainingMoves.tail
            val currentValue = modifiedNegamax(state, nextMove, nextState, depth - 1, beta.invert, currentAlpha.invert).invert
            bestValue = if (bestValue > currentValue) bestValue else currentValue
            currentAlpha = if (currentAlpha > currentValue) currentAlpha else currentValue
          }
          if (remainingMoves.nonEmpty) counters.betaCutoffs += 1
          logger.debug(s"Alpha-beta pruned minimax evaluation: $bestValue")
          bestValue
      }
    }
  }

  override def findBestMove(state: S, depth: Int): S#Move = {

    val (bestMove, duration) = time {
      val moves = possibleMoves(state)
      var bestMove = moves.head._1
      var bestValue: Score = Result(Double.MinValue)
      var currentAlpha: Score = Result(Double.MinValue)
      for((move, newState) <- moves) {
        val currentValue = modifiedNegamax(state, move, newState, depth - 1, Result(Double.MinValue), currentAlpha.invert).invert
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
