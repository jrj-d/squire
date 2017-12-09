package squire.chess

import scala.io.{BufferedSource, Source}
import org.scalatest.FunSpec


trait PerftBehaviors { this: FunSpec =>

  // count the number of (path, en passant, castling)
  def countMoves(state: ChessState, depth: Int): MoveCount = depth match {

    case 1 => {
      val moves = state.possibleMoves
      val counts = moves.map {
        case _: EnPassant => MoveCount.enPassant
        case _: Castling => MoveCount.castling
        case _:Promotion => MoveCount.promotion
        case _ => MoveCount.regular
      }.foldLeft(MoveCount.empty)(_ + _)
      val checks = moves.map(m => {
        val newState = state.apply(m)
        if(newState.isInCheck(Color.fromPlayer(newState.currentPlayer))) {
          if(newState.possibleMoves.isEmpty) {
            MoveCount.checkMate
          }
          else {
            MoveCount.check
          }
        } else {
          MoveCount.empty
        }
      }).foldLeft(MoveCount.empty)(_ + _)
      counts + checks
    }

    case d => {
      val moves = state.possibleMoves
      moves.map(m => countMoves(state.apply(m), d - 1)).foldLeft(MoveCount.empty)(_ + _)
    }
  }

  def perftTestedEngine(state: ChessState, depth: Int, goal: MoveCount): Unit = {

    val result = countMoves(state, depth)

    it("should find " + goal.leaves + " leaf nodes") {
      assert(result.leaves == goal.leaves)
    }

    it("should find " + goal.enPassant + " en passant moves at leaf nodes") {
      assert(result.enPassant == goal.enPassant)
    }

    it("should find " + goal.castling + " castling moves at leaf nodes") {
      assert(result.castling == goal.castling)
    }

    it("should find " + goal.promotion + " promotion moves at leaf nodes") {
      assert(result.promotion == goal.promotion)
    }

    it("should find " + goal.check + " check moves at leaf nodes") {
      assert(result.check == goal.check)
    }

    it("should find " + goal.checkMate + " checkmate moves at leaf nodes") {
      assert(result.checkMate == goal.checkMate)
    }
  }
}

class PerftTest extends FunSpec with PerftBehaviors {

  val source: BufferedSource = Source.fromURL(getClass.getResource("/perfts-for-rules.fen"))
  val perfts: Seq[Perft] = readPerftFile(source).toList

  for(perft <- perfts) {

    val initState = ChessState.parseFen(perft.fen)
    val alternateState = new ChessState(initState.ply, initState.halfMoveClock, initState.board,
                                        initState.positions, initState.castlingRights, initState.enPassantPosition) {
      override def isInCheck(color: Color): Boolean = {
        val kingPosition = positions(ChessPiece(color, King, 0))
        positions.keys.filter(_.color != color).map(threatens(_, kingPosition)).reduceLeft(_ || _)
      }
    }

    describe("From " + perft.fen + " at depth " + perft.depth + ", the engine") {
      it should behave like perftTestedEngine(initState, perft.depth, perft.moveCount)
    }

    describe("From " + perft.fen + " at depth " + perft.depth + ", the alternate engine using method threatens()") {
      it should behave like perftTestedEngine(alternateState, perft.depth, perft.moveCount)
    }
  }

}
