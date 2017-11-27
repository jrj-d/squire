package squire.chess

import scala.io.{BufferedSource, Source}
import org.scalatest.FunSpec

case class MoveCount(
                      leaves: Long,
                      enPassant: Long,
                      castling: Long,
                      promotion: Long,
                      check: Long,
                      checkMate: Long) {

  def +(c: MoveCount): MoveCount = MoveCount(
    this.leaves + c.leaves,
    this.enPassant + c.enPassant,
    this.castling + c.castling,
    this.promotion + c.promotion,
    this.check + c.check,
    this.checkMate + c.checkMate
  )
}

object MoveCount {

  def fromSeq(l: Seq[Long]): MoveCount = MoveCount(l(0), l(1), l(2), l(3), l(4), l(5))

  def empty: MoveCount = MoveCount(0, 0, 0, 0, 0, 0)
  def enPassant: MoveCount = MoveCount(1, 1, 0, 0, 0, 0)
  def castling: MoveCount = MoveCount(1, 0, 1, 0, 0, 0)
  def promotion: MoveCount = MoveCount(1, 0, 0, 1, 0, 0)
  def regular: MoveCount = MoveCount(1, 0, 0, 0, 0, 0)
  def check: MoveCount = MoveCount(0, 0, 0, 0, 1, 0)
  def checkMate: MoveCount = MoveCount(0, 0, 0, 0, 1, 1)
}

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

  def perftTestedEngine(state: ChessState, depth: Int, goal: MoveCount) {

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

class Perft extends FunSpec with PerftBehaviors {

  val perfts: BufferedSource = Source.fromURL(getClass.getResource("/perfts.fen"))

  for(perft <- perfts.getLines if !perft.startsWith("#")) {
    val words = perft.split(" ")
    val fen = words.slice(0, 6).reduceLeft(_ + " " + _)
    val depth = words(6).toInt
    val moveCount = MoveCount.fromSeq(words.slice(7, 13).map(_.toLong))
    val initState = ChessState.parseFen(fen)
    val alternateState = new ChessState(initState.currentPlayer, initState.board, initState.positions, initState.castlingRights, initState.enPassantPosition) {
      override def isInCheck(color: Color): Boolean = {
        val kingPosition = positions(ChessPiece(color, King, 0))
        positions.keys.filter(_.color != color).map(threatens(_, kingPosition)).reduceLeft(_ || _)
      }
    }

    describe("From " + fen + " at depth " + depth + ", the engine") {
      it should behave like perftTestedEngine(initState, depth, moveCount)
    }

    describe("From " + fen + " at depth " + depth + ", the alternate engine using method threatens()") {
      it should behave like perftTestedEngine(alternateState, depth, moveCount)
    }
  }

}
