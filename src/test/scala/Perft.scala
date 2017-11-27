package old

import scala.io.Source
import org.scalatest.FunSpec
import old.Color._

case class MoveCount(val leaves: Long,
					 val enPassant: Long,
					 val castling: Long,
					 val promotion: Long,
					 val check: Long,
					 val checkMate: Long) {

	def this() = this(0, 0, 0, 0, 0, 0)

	def this(l: Array[Long]) = this(l(0), l(1), l(2), l(3), l(4), l(5))

	def + (c: MoveCount) = new MoveCount(this.leaves + c.leaves,
										 this.enPassant + c.enPassant,
										 this.castling + c.castling,
										 this.promotion + c.promotion,
										 this.check + c.check,
										 this.checkMate + c.checkMate)
}

trait PerftBehaviors { this: FunSpec =>

	// count the number of (path, en passant, castling)
	def countMoves(state: ChessState, depth: Int): MoveCount = depth match {

		case 1 => {
			val moves = state.possibleMoves
			val counts = if(moves.length == 0) new MoveCount else moves.map(_ match {
				case _: EnPassant => MoveCount(1, 1, 0, 0, 0, 0)
				case _: Castling => MoveCount(1, 0, 1, 0, 0, 0)
				case _: Promotion => MoveCount(1, 0, 0, 1, 0, 0)
				case _ => MoveCount(1, 0, 0, 0, 0, 0)
			}).reduceLeft(_ + _)
			val checks = if(moves.length == 0) new MoveCount else moves.map(m => {
				val newState = state.apply(m)
				val color = if(newState.turn % 2 == 0) White else Black
				if(newState.isInCheck(color)) {
					if(newState.possibleMoves.length == 0) MoveCount(0, 0, 0, 0, 1, 1)
					else MoveCount(0, 0, 0, 0, 1, 0)
				} else new MoveCount
			}).reduceLeft(_ + _)
			counts + checks
		}

		case d => {
			val moves = state.possibleMoves
			if(moves.length == 0) new MoveCount
			else moves.map(m => countMoves(state.apply(m), d - 1)).reduceLeft(_ + _)
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

	val perfts = Source.fromURL(getClass.getResource("/perfts.fen"))

	for(perft <- perfts.getLines if !perft.startsWith("#")) {
		val words = perft.split(" ")
		val fen = words.slice(0, 6).reduceLeft(_ + " " + _)
		val depth = words(6).toInt
		val moveCount = new MoveCount(words.slice(7, 13).map(_.toLong))
		val initState = ChessState.parseFen(fen)
		val alternateState = new ChessState(initState.turn, initState.board, initState.positions, initState.castlingRights, initState.enPassantPosition) {
			override def isInCheck(color: Color) = {
		        val king_pos = positions(King(color))
		        positions.keys.filter(_.color != color).map(threatens(_, king_pos)).reduceLeft(_ || _)
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
