package squire.agents.minimax

import com.typesafe.scalalogging.LazyLogging
import squire.base.{Agent, Finished, Playing, State}
import squire.utils.time

class NegamaxAgent[S <: State[S]](heuristic: S => Double, maxDepth: Int) extends Agent[S] with LazyLogging {

  def play(state: S): S#Move = findBestMove(state, maxDepth)

  def findBestMove(state: S, depth: Int): S#Move = {

    val counters = new Counters

    def negamax(state: S, depth: Int): Score = {
      counters.traversedNodes += 1
      logger.debug(s"Remaining depth is $depth, evaluating\n$state")
      state.evaluate match {
        case Finished(result) => {
          logger.debug(s"State is final: $result")
          counters.finalNodes += 1
          Result(result)
        }
        case Playing =>
          if(depth == 0) {
            val value = heuristic(state)
            counters.evaluatedHeuristics += 1
            logger.debug(s"Heuristic evaluation: $value")
            Heuristic(value)
          } else {
            val value = state.possibleMoves
              .map(state.apply)
              .map(negamax(_, depth - 1).invert)
              .max
            logger.debug(s"Minimax evaluation: $value")
            value
          }
      }
    }

    val (bestMove, duration) = time {
      state.possibleMoves
        .map { m => (m, negamax(state.apply(m), depth - 1).invert) }
        .maxBy(_._2)
        ._1
    }

    logger.info(f"Best move at $depth-ply depth is $bestMove")
    logger.info(s"\n${counters.print(duration)}")

    bestMove
  }

}
