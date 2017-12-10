package squire.chess

import org.scalatest.FunSpec
import squire.agents.minimax.{AlphaBetaNegamaxAgent, MovesOrdering, NegamaxAgent, Result}

import scala.io.{BufferedSource, Source}
import scala.util.Random

class AlphaBetaTest extends FunSpec {

  def equalAgents(state: ChessState, depth: Int): Unit = {

    val generator = new Random(42)

    val randomNumbers = Seq.fill(64)(generator.nextDouble)

    def heuristic(state: ChessState): Double = {
      val whiteValue = state.board.flatten.zip(randomNumbers).map { case (pieceOption, r) =>
        pieceOption match {
          case None => 0
          case Some(piece) if piece.color == White => r
          case _ => -r
        }
      }.reduceLeft(_ + _)
      if(state.currentPlayer == 0) {
        whiteValue
      } else {
        -whiteValue
      }
    }

    val normalAgent = new NegamaxAgent[ChessState](heuristic, depth)
    val abAgent = new AlphaBetaNegamaxAgent[ChessState](heuristic, depth)
    val movesOrderingABAgent = new AlphaBetaNegamaxAgent[ChessState](heuristic, depth) with MovesOrdering[ChessState]

    val normalValue = normalAgent.negamax(state, depth)
    val abValue = abAgent.negamax(state, depth, Result(Double.MinValue), Result(Double.MaxValue))
    val moabValue = movesOrderingABAgent.negamax(state, depth, Result(Double.MinValue), Result(Double.MaxValue))

    it(s"negamax and alpha beta should output the same value $normalValue") {
      assert(normalValue == abValue)
    }
    it(s"negamax and ordered alpha beta should output the same value $normalValue") {
      assert(normalValue == moabValue)
    }
  }

  val source: BufferedSource = Source.fromURL(getClass.getResource("/perfts-for-ab.fen"))
  val perfts: Seq[Perft] = readPerftFile(source).toList

  for(perft <- perfts) {

    val state = ChessState.parseFen(perft.fen)

    describe("For " + perft.fen + " at depth " + perft.depth + ",") {
      it should behave like equalAgents(state, perft.depth)
    }

  }

}
