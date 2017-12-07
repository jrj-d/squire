package squire.chess

import org.scalatest.FunSpec
import squire.agents.minimax.{AlphaBetaNegamaxAgent, NegamaxAgent}

import scala.io.{BufferedSource, Source}

class AlphaBetaTest extends FunSpec {

  def equalAgents(state: ChessState, depth: Int): Unit = {

    val randomNumbers = Seq.fill(64)(scala.util.Random.nextDouble)

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

    val normalMove = normalAgent.play(state)
    val abMove = abAgent.play(state)

    it(s"should find the same best move $normalMove") {
      assert(normalMove == abMove)
    }
  }

  val source: BufferedSource = Source.fromURL(getClass.getResource("/perfts-for-ab.fen"))
  val perfts: Seq[Perft] = readPerftFile(source).toList

  for(perft <- perfts) {

    val state = ChessState.parseFen(perft.fen)

    describe("From " + perft.fen + " at depth " + perft.depth + ", normal negamax and alpha beta negamax agents") {
      it should behave like equalAgents(state, perft.depth)
    }

  }

}
