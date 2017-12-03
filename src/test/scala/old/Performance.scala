package old

import old.Color.Color

import scala.io.{BufferedSource, Source}

object Performance {

  def time[R](block: => R): (R, Double) = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    (result, (t1 - t0) / 1e6)
  }

  def countMoves(state: ChessState, depth: Int): Int = depth match {

    case 1 => {
      val moves = state.possibleMoves
      moves.length
    }

    case d => {
      val moves = state.possibleMoves
      moves.map(m => countMoves(state.apply(m), d - 1)).reduceLeft(_ + _)
    }
  }

  def main(args: Array[String]): Unit = {
    val perfts: BufferedSource = Source.fromURL(getClass.getResource("/perfts-for-rules.fen"))

    for(perft <- perfts.getLines if !perft.startsWith("#")) {
      val words = perft.split(" ")
      val fen = words.slice(0, 6).reduceLeft(_ + " " + _)
      val depth = words(6).toInt
      val initState = ChessState.parseFen(fen)
      val alternateState = new ChessState(initState.turn, initState.board, initState.positions, initState.castlingRights, initState.enPassantPosition) {
        override def isInCheck(color: Color) = {
          val king_pos = positions(King(color))
          positions.keys.filter(_.color != color).map(threatens(_, king_pos)).reduceLeft(_ || _)
        }
      }

      val (nLeaves, duration) = time(countMoves(initState, depth))
      val (nLeavesAlternate, durationAlternate) = time(countMoves(alternateState, depth))

      println(s"Fen $fen at depth $depth run in $duration ms for $nLeaves leaves with normal engine")
      println(s"Fen $fen at depth $depth run in $durationAlternate ms for $nLeavesAlternate leaves with alternate engine using method threatens()")
    }
  }

}
