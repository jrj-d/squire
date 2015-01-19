package squire.chess

import scala.io.Source
import org.scalatest.FlatSpec
import Color._

class Perft extends FlatSpec {

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
			else state.possibleMoves.map(m => countMoves(state.apply(m), d - 1)).reduceLeft(_ + _)
		}
	}

	val perfts = Source.fromURL(getClass.getResource("/perfts.fen"))

	for(perft <- perfts.getLines) {
		val words = perft.split(" ")
		val fen = words.slice(0, 6).reduceLeft(_ + " " + _)
		val depth = words(6).toInt
		val moveCount = new MoveCount(words.slice(7, 13).map(_.toLong))
		val initState = ChessState.parseFen(fen)

		val resMoveCount = countMoves(initState, depth)

		("From " + fen + " at depth " + depth + ", the engine") should ("find " + moveCount.leaves + " leaves") in {
			assert(resMoveCount.leaves == moveCount.leaves)
		}

		it should ("find " + moveCount.enPassant + " en passant moves at leaf nodes") in {
			assert(resMoveCount.enPassant == moveCount.enPassant)
		}

		it should ("find " + moveCount.castling + " castling moves at leaf nodes") in {
			assert(resMoveCount.castling == moveCount.castling)
		}

		it should ("find " + moveCount.promotion + " promotion moves at leaf nodes") in {
			assert(resMoveCount.promotion == moveCount.promotion)
		}

		it should ("find " + moveCount.check + " check moves at leaf nodes") in {
			assert(resMoveCount.check == moveCount.check)
		}

		it should ("find " + moveCount.checkMate + " checkmate moves at leaf nodes") in {
			assert(resMoveCount.checkMate == moveCount.checkMate)
		}
	}

}
