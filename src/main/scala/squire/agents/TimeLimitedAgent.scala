package squire.agents

import java.util.concurrent.TimeUnit.MILLISECONDS

import com.typesafe.scalalogging.LazyLogging
import squire.base.{Agent, State}
import squire.utils.time

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Future, TimeoutException}
import scala.util.{Failure, Success, Try}

class TimeLimitedAgent[S <: State[S], C](baseAgentBuilder: C => Agent[S], complexities: Seq[C], timeInMillis: Long) extends Agent[S] with LazyLogging {

  // The behavior of this agent is not perfect: it will return the best move when time is up,
  // but the computation Future will continue until a given "complexity" has been computed.
  // This might hog a fair share of CPU power. The reason for this is that I found it dirty
  // to embarass the code of specific agents like negamax with interruption-related code.

  def play(state: S): S#Move = {

    val startTime = System.currentTimeMillis
    val maxTime = startTime + timeInMillis

    var bestMove: S#Move = baseAgentBuilder(complexities.head).play(state)

    val interruptableWork: Future[S#Move] = Future {
      var remainingComplexities = complexities.tail
      var lastDuration = 0.0
      while (remainingComplexities.nonEmpty && lastDuration < maxTime - System.currentTimeMillis) {
        val complexity = remainingComplexities.head
        logger.info(s"Started move computation of complexity $complexity")
        val (move, duration) = time(baseAgentBuilder(complexity).play(state))
        logger.info(s"Completed move computation of complexity $complexity")
        lastDuration = duration
        remainingComplexities = remainingComplexities.tail
        synchronized {
          bestMove = move
        }
      }
      logger.info(s"Future is closed")
      bestMove
    }

    val remainingDuration = timeInMillis - (System.currentTimeMillis - startTime)

    Try(Await.result(interruptableWork, FiniteDuration(remainingDuration, MILLISECONDS))) match {
      case Success(move) =>
        logger.info(s"Future decided to finish before alloted time of $timeInMillis ms is up.")
        move
      case Failure(e: TimeoutException) =>
        logger.info(s"Alloted time of $timeInMillis ms is finished, returning computed move with highest complexity")
        bestMove
      case Failure(e) => throw e
    }

  }

}
