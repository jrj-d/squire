package squire.chess

import com.typesafe.scalalogging.LazyLogging

package object heuristics extends LazyLogging {

  // scalastyle:off magic.number
  def tradeValue(state: ChessState): Double = {

    val boardValue = state.positions.keys.toSeq.map { piece =>
      val value = piece.pieceType match {
        case King => 0
        case Queen => 9
        case Bishop => 3
        case Knight => 3
        case Rook => 5
        case Pawn => 1
      }
      if(piece.color == White) {
        value
      } else {
        -value
      }
    }.reduceLeft(_ + _) // faster than sum

    if(state.currentPlayer == 0) {
      boardValue
    } else {
      -boardValue
    }

  }
  // scalastyle:on

}
