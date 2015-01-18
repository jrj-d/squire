package squire.chess

import scala.io.Source
import org.scalatest.FlatSpec

class Perft extends FlatSpec {

	val perfts = Source.fromURL(getClass.getResource("/perfts.fen"))

	def countPaths(state: ChessState, depth: Int): Long = depth match {
		case 0 => 1
		case d => state.possibleMoves.map(m => countPaths(state.apply(m), d - 1)).sum
	}

	var usePronoun = false

	for(perft <- perfts.getLines) {
		val words = perft.split(" ")
		val fen = words.slice(0, 6).reduceLeft(_ + " " + _)
		val depth = words(6).toInt
		val nPaths = words(7).toLong
		val initState = ChessState.parseFen(fen)
		if(usePronoun) {
			it should ("find " + nPaths + " move paths at depth " + depth + " from " + fen) in {
				assert(countPaths(initState, depth) == nPaths)
			}
		} else {
			"The engine" should ("find " + nPaths + " move paths at depth " + depth + " from " + fen) in {
				assert(countPaths(initState, depth) == nPaths)
			}
		}
		usePronoun = true
	}

}
