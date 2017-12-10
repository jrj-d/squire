package squire.chess

import com.typesafe.scalalogging.LazyLogging
import scala.{IndexedSeq => I}

// scalastyle:off magic.number
// scalastyle:off cyclomatic.complexity

// michniewski heuristic comes from https://chessprogramming.wikispaces.com/Simplified+evaluation+function

package object heuristics extends LazyLogging {

  private val pieceSquareTable = Map[PieceType, I[I[Int]]](
    Pawn -> I(
      I(0,  0,  0,  0,  0,  0,  0,  0),
      I(50, 50, 50, 50, 50, 50, 50, 50),
      I(10, 10, 20, 30, 30, 20, 10, 10),
      I(5,  5, 10, 25, 25, 10,  5,  5),
      I(0,  0,  0, 20, 20,  0,  0,  0),
      I(5, -5,-10,  0,  0,-10, -5,  5),
      I(5, 10, 10,-20,-20, 10, 10,  5),
      I(0,  0,  0,  0,  0,  0,  0,  0)
    ),
    Knight -> I(
      I(-50,-40,-30,-30,-30,-30,-40,-50),
      I(-40,-20,  0,  0,  0,  0,-20,-40),
      I(-30,  0, 10, 15, 15, 10,  0,-30),
      I(-30,  5, 15, 20, 20, 15,  5,-30),
      I(-30,  0, 15, 20, 20, 15,  0,-30),
      I(-30,  5, 10, 15, 15, 10,  5,-30),
      I(-40,-20,  0,  5,  5,  0,-20,-40),
      I(-50,-40,-30,-30,-30,-30,-40,-50)
    ),
    Bishop -> I(
      I(-20,-10,-10,-10,-10,-10,-10,-20),
      I(-10,  0,  0,  0,  0,  0,  0,-10),
      I(-10,  0,  5, 10, 10,  5,  0,-10),
      I(-10,  5,  5, 10, 10,  5,  5,-10),
      I(-10,  0, 10, 10, 10, 10,  0,-10),
      I(-10, 10, 10, 10, 10, 10, 10,-10),
      I(-10,  5,  0,  0,  0,  0,  5,-10),
      I(-20,-10,-10,-10,-10,-10,-10,-20)
    ),
    Rook -> I(
      I(0,  0,  0,  0,  0,  0,  0,  0),
      I(5, 10, 10, 10, 10, 10, 10,  5),
      I(-5,  0,  0,  0,  0,  0,  0, -5),
      I(-5,  0,  0,  0,  0,  0,  0, -5),
      I(-5,  0,  0,  0,  0,  0,  0, -5),
      I(-5,  0,  0,  0,  0,  0,  0, -5),
      I(-5,  0,  0,  0,  0,  0,  0, -5),
      I(0,  0,  0,  5,  5,  0,  0,  0)
    ),
    Queen -> I(
      I(-20,-10,-10, -5, -5,-10,-10,-20),
      I(-10,  0,  0,  0,  0,  0,  0,-10),
      I(-10,  0,  5,  5,  5,  5,  0,-10),
      I(-5,  0,  5,  5,  5,  5,  0, -5),
      I(0,  0,  5,  5,  5,  5,  0, -5),
      I(-10,  5,  5,  5,  5,  5,  0,-10),
      I(-10,  0,  5,  0,  0,  0,  0,-10),
      I(-20,-10,-10, -5, -5,-10,-10,-20)
    )
  )

  private val kingPieceSquareTable = I(
    I( // middle game
      I(-30,-40,-40,-50,-50,-40,-40,-30),
      I(-30,-40,-40,-50,-50,-40,-40,-30),
      I(-30,-40,-40,-50,-50,-40,-40,-30),
      I(-30,-40,-40,-50,-50,-40,-40,-30),
      I(-20,-30,-30,-40,-40,-30,-30,-20),
      I(-10,-20,-20,-20,-20,-20,-20,-10),
      I(20, 20,  0,  0,  0,  0, 20, 20),
      I(20, 30, 10,  0,  0, 10, 30, 20)
    ),
    I( // end game
      I(-50,-40,-30,-20,-20,-30,-40,-50),
      I(-30,-20,-10,  0,  0,-10,-20,-30),
      I(-30,-10, 20, 30, 30, 20,-10,-30),
      I(-30,-10, 30, 40, 40, 30,-10,-30),
      I(-30,-10, 30, 40, 40, 30,-10,-30),
      I(-30,-10, 20, 30, 30, 20,-10,-30),
      I(-30,-30,  0,  0,  0,  0,-30,-30),
      I(-50,-30,-30,-30,-30,-30,-30,-50)
    )
  )


  private def pieceValue(piece: ChessPiece): Int = {
    val value = piece.pieceType match {
      case King => 0
      case Queen => 900
      case Bishop => 330
      case Knight => 320
      case Rook => 500
      case Pawn => 100
    }
    if(piece.color == White) {
      value
    } else {
      -value
    }
  }

  private def endGameContribution(piece: PieceType): Int = piece match {
    case Pawn | King => 0
    case Queen => 6
    case Bishop => 1
    case Knight => 1
    case Rook => 3
  }

  private def endGameGradient(state: ChessState): Double = {
    val startGameScore = state.positions.keys.toSeq.map(p => endGameContribution(p.pieceType)).reduceLeft(_ + _)
    1.0 - startGameScore / 32.0
  }

  private def piecePosition(piece: ChessPiece, position: Position, endGameGradient: Double): Double = {
    piece.pieceType match {
      case King if piece.color == White =>
        (1.0 - endGameGradient) * kingPieceSquareTable(0)(7 - position.row)(position.column)
        + endGameGradient * kingPieceSquareTable(1)(7 - position.row)(position.column)
      case King =>
        (-1.0 + endGameGradient) * kingPieceSquareTable(0)(position.row)(position.column)
        - endGameGradient * kingPieceSquareTable(1)(position.row)(position.column)
      case p if piece.color == White => pieceSquareTable(p)(7 - position.row)(position.column).toDouble
      case p => -pieceSquareTable(p)(position.row)(position.column).toDouble
    }
  }

  def tradeValue(state: ChessState): Double = {

    val boardValue = state.positions.keys.toSeq.map(pieceValue).reduceLeft(_ + _) // faster than sum

    if(state.currentPlayer == 0) {
      boardValue
    } else {
      -boardValue
    }

  }

  def michniewski(state: ChessState): Double = {

    val gradient = endGameGradient(state)

    val boardValue = state.positions.map { case (piece, position) =>
      pieceValue(piece) + piecePosition(piece, position, gradient)
    }.reduceLeft(_ + _) // faster than sum

    if(state.currentPlayer == 0) {
      boardValue
    } else {
      -boardValue
    }

  }

  def isNotCaptureNorCheckNorPromotion(state: ChessState, move: ChessMove, nextState: ChessState): Boolean = move match {
    case Promotion(_, _, _) => false
    case Castling(_, _) => true
    case EnPassant(_, _) => false
    case RegularChessMove(_, arrival) if state.getPiece(arrival).nonEmpty => false
    case _ => !nextState.isInCheck()
  }

}
