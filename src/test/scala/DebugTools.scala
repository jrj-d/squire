package squire

import old.ChessState

package object debug {

	import chess._

	def countMoves(state: ChessState, depth: Int): Long = depth match {

		case 0 => 1

		case d => {
			val moves = state.possibleMoves
			if(moves.length == 0) 0
			else moves.map(m => countMoves(state.apply(m), d - 1)).reduceLeft(_ + _)
		}
	}

	def divide(state: ChessState, depth: Int): Unit = if(depth < 1) println("depth should be higher") else {
		val moves = state.possibleMoves
		val counts = moves.map(state.encodeANMove(_)).zip(moves.map(m => countMoves(state.apply(m), depth - 1))).sortBy(_._1)
		for((m, c) <- counts) {
			println(m + ": " + c)
		}
		println("Total: " + counts.map(_._2).reduceLeft(_ + _))
	}

}
