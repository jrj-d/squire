package squire.chess

sealed trait PieceType
case object Pawn extends PieceType
case object King extends PieceType
case object Queen extends PieceType
case object Rook extends PieceType
case object Knight extends PieceType
case object Bishop extends PieceType


case class ChessPiece(color: Color, pieceType: PieceType, id: Int)


case class Position(row: Int, column: Int)


sealed abstract class ChessMove
case class RegularChessMove(origin: Position, destination: Position) extends ChessMove
case class Castling(kingPosition: Position, rookPosition: Position) extends ChessMove
case class Promotion(origin: Position, promoted: PieceType, destination: Position) extends ChessMove
case class EnPassant(origin: Position, destination: Position) extends ChessMove
