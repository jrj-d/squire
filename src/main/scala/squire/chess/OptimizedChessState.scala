package squire.chess

import squire.base.{Evaluation, Finished, Playing, State}

import scala.collection.immutable.{Map => ImmutableMap}
import scala.collection.mutable
import scala.collection.mutable.{IndexedSeq => MutableIndexedSeq, Map => MutableMap}
import scala.math.abs
import scala.util.matching.Regex

case class OptimizedChessState(
                       currentPlayer: Int,
                       board: Array[Array[Option[ChessPiece]]],
                       positions: ImmutableMap[ChessPiece, Position],
                       castlingRights: ImmutableMap[Color, IndexedSeq[Boolean]],
                       enPassantPosition: Option[Position]
                     ) extends State[OptimizedChessState] {

  type Move = ChessMove

  private def getPiece(pos: Position): Option[ChessPiece] = board(pos.row)(pos.column)

  // scalastyle:off

  override def apply(move: ChessMove): OptimizedChessState = {

    // This method does not test if the move is a valid move.
    // The moves returned by the method possibleMoves() are all valid.
    // The tests done here only ensure the integrity of the pointers between
    // the board and the positions map.
    // The expected behavior is: throw an exception if the move is not valid.
    // That's why there are some unsafe pieces of code.

    val newBoard = board.map(_.clone)
    val newCastlingRights = MutableMap(castlingRights.mapValues(row => MutableIndexedSeq(row:_*)).toSeq:_*)
    val newPositions = MutableMap(positions.toSeq:_*)
    var newEnPassantPosition: Option[Position] = None

    move match {

      case RegularChessMove(origin, destination) => {

        val piece = getPiece(origin).get // fail if no moved piece

        // remove deleted piece if needed
        val deletedPieceOption = getPiece(destination)
        deletedPieceOption.foreach { deletedPiece =>
          if(deletedPiece.pieceType == King) throw new IllegalArgumentException("cannot capture king")
          newPositions -= deletedPiece
        }

        // move piece
        newBoard(destination.row)(destination.column) = Some(piece)
        newBoard(origin.row)(origin.column) = None
        newPositions(piece) = destination

        // handle castling rights
        piece.pieceType match {
          case King =>
            newCastlingRights(piece.color)(0) = false
            newCastlingRights(piece.color)(1) = false
          case Rook =>
            newCastlingRights(piece.color)(piece.id) = false
          case _ => ()
        }

        // handle castling rights if a rook is captured
        deletedPieceOption.foreach { deletedPiece =>
          if (deletedPiece.pieceType == Rook & deletedPiece.id <= 1) {
            newCastlingRights(deletedPiece.color)(deletedPiece.id) = false
          }
        }

        // handle en passant marking
        newEnPassantPosition = piece.pieceType match {
          case Pawn =>
            if(abs(origin.row - destination.row) == 2) {
              val dir = if(piece.color == White) 1 else -1
              Some(Position(destination.row - dir, destination.column))
            } else None
          case _ => None
        }

      }

      case Castling(kingPos, rookPos) => {

        val kingOption = getPiece(kingPos)
        val rookOption = getPiece(rookPos)
        if(!kingOption.exists(_.pieceType == King)) throw new IllegalArgumentException("castling: king is not where indicated")
        if(!rookOption.exists(_.pieceType == Rook)) throw new IllegalArgumentException("castling: there is no rook where indicated")
        val king = kingOption.get
        val rook = rookOption.get

        val row = if(king.color == White) 0 else 7

        if(kingPos.row != row || kingPos.column != 4) throw new IllegalArgumentException("castling: king not in the right place")
        if(rookPos.row != row) throw new IllegalArgumentException("castling: rook not in the right row")

        if(rookPos.column == 0) {

          newBoard(row)(4) = None
          newBoard(row)(0) = None

          if(board(row)(3).isDefined || board(row)(2).isDefined) throw new IllegalArgumentException("castling: some pieces between rook and king")

          newBoard(row)(3) = Some(rook)
          newBoard(row)(2) = Some(king)
          newPositions(rook) = Position(row, 3)
          newPositions(king) = Position(row, 2)

          newCastlingRights(king.color)(0) = false
          newCastlingRights(king.color)(1) = false

        } else if(rookPos.column == 7) {

          newBoard(row)(4) = None
          newBoard(row)(7) = None

          if(board(row)(5).isDefined || board(row)(6).isDefined) throw new IllegalArgumentException("castling: some pieces between rook and king")

          newBoard(row)(5) = Some(rook)
          newBoard(row)(6) = Some(king)
          newPositions(rook) = Position(row, 5)
          newPositions(king) = Position(row, 6)

          newCastlingRights(king.color)(0) = false
          newCastlingRights(king.color)(1) = false

        } else throw new IllegalArgumentException(s"castling: rook is positioned on a bad column (${rookPos.column})")
      }

      case Promotion(origin, promoted, destination) => {

        val pawn = getPiece(origin).get // fail if no piece
        if(pawn.pieceType != Pawn) throw new IllegalArgumentException("promotion: piece is not a pawn")

        // remove deleted piece if needed
        val deletedPieceOption = getPiece(destination)
        deletedPieceOption.foreach { deletedPiece =>
          if(deletedPiece.pieceType == King) throw new IllegalArgumentException("cannot capture king")
          newPositions -= deletedPiece
        }

        val newPiece = promoted match {
          case t @ (Rook | Knight | Bishop | Queen) => ChessPiece(pawn.color, t, 8 + pawn.id) // just to be sure in case of perft
          case t => throw new IllegalArgumentException(s"cannot promote pawn to $t")
        }

        newBoard(origin.row)(origin.column) = None
        newBoard(destination.row)(destination.column) = Some(newPiece)
        newPositions -= pawn
        newPositions(newPiece) = destination
      }

      case EnPassant(origin, destination) => {

        val piece = getPiece(origin).get // fail if no piece
        val deletedPiece = getPiece(Position(origin.row, destination.column)).get // fail if no piece

        newBoard(destination.row)(destination.column) = Some(piece)
        newBoard(origin.row)(destination.column) = None
        newBoard(origin.row)(origin.column) = None
        newPositions(piece) = destination
        newPositions -= deletedPiece
      }
    }

    OptimizedChessState(
      (currentPlayer + 1) % 2,
      newBoard,
      ImmutableMap(newPositions.toSeq:_*),
      ImmutableMap(newCastlingRights.mapValues(row => IndexedSeq(row:_*)).toSeq:_*),
      newEnPassantPosition
    )

  }

  def threatens(piece: ChessPiece, destination: Position): Boolean = {

    val pos = positions(piece)

    val dx = destination.row - pos.row
    val dy = destination.column - pos.column
    val dir_x = if(dx >= 0) 1 else -1
    val dir_y = if(dy >= 0) 1 else -1

    piece.pieceType match {
      case Pawn => {
        val direction = piece.color match {
          case White => 1
          case Black => -1
        }
        (dx == direction) && (abs(dy) == 1)
        // en passant is missing
      }
      case Rook => {
        if(dx != 0 && dy != 0) false
        else if(dy == 0) {
          (1 until abs(dx)).forall(d => board(pos.row + dir_x * d)(pos.column).isEmpty)
        } else {
          (1 until abs(dy)).forall(d => board(pos.row)(pos.column + dir_y * d).isEmpty)
        }
      }
      case Knight => (abs(dx) == 2 && abs(dy) == 1) || (abs(dx) == 1 && abs(dy) == 2)
      case Bishop => {
        if(abs(dx) != abs(dy)) false
        else {
          (1 until abs(dx)).forall(d => board(pos.row + dir_x * d)(pos.column + dir_y * d).isEmpty)
        }
      }
      case Queen => {
        if(dy == 0) {
          (1 until abs(dx)).forall(d => board(pos.row + dir_x * d)(pos.column).isEmpty)
        } else if(dx == 0) {
          (1 until abs(dy)).forall(d => board(pos.row)(pos.column + dir_y * d).isEmpty)
        } else if(abs(dx) == abs(dy)) {
          (1 until abs(dx)).forall(d => board(pos.row + dir_x * d)(pos.column + dir_y * d).isEmpty)
        } else false
      }
      case King => abs(dx) <= 1 && abs(dy) <= 1
    }
  }

  def isThreatened(position: Position, color: Color): Boolean = {

    def withinBoard(x: Int) = x >= 0 && x <= 7

    val dir = if(color == White) 1 else -1

    // pawns
    if(withinBoard(position.row + dir)) {
      for(dx <- -1 to 1 by 2) {
        if(withinBoard(position.column + dx)) {
          board(position.row + dir)(position.column + dx).foreach { piece =>
            if(piece.pieceType == Pawn && piece.color != color) return true
          }
        }
      }
    }

    // knights
    for(dx <- -2 to 2 if dx != 0) {
      val abs_dy = 3 - abs(dx)
      for(dir <- -1 to 1 by 2) {
        val dy = dir * abs_dy
        if(withinBoard(position.row + dx) && withinBoard(position.column + dy)) {
          board(position.row + dx)(position.column + dy).foreach { piece =>
            if(piece.pieceType == Knight && piece.color != color) return true
          }
        }
      }
    }

    // the rest
    def moveInDirection(dx: Int, dy: Int) = {
      val Position(x, y) = position
      val (threatX, threatY) = Stream.from(1).map(m => (x + m * dx, y + m * dy)).dropWhile { case (newX, newY) =>
        withinBoard(newX) && withinBoard(newY) && board(newX)(newY).isEmpty
      }.head

      if(withinBoard(threatX) && withinBoard(threatY) && board(threatX)(threatY).get.color != color) {
        val piece = board(threatX)(threatY).get
        if(dx == 0 || dy == 0) {
          piece.pieceType match {
            case King => abs(threatX - x) <= 1 && abs(threatY - y) <= 1
            case Queen | Rook => true
            case _ => false
          }
        } else {
          piece.pieceType match {
            case King => abs(threatX - x) <= 1 && abs(threatY - y) <= 1
            case Queen | Bishop => true
            case _ => false
          }
        }
      } else false
    }

    for(d <- -1 to 1 by 2) {
      if(moveInDirection(d, 0)) return true
      if(moveInDirection(0, d)) return true
    }
    for(dir_x <- -1 to 1 by 2) {
      for(dir_y <- -1 to 1 by 2) {
        if(moveInDirection(dir_x, dir_y)) return true
      }
    }

    false
  }

  def isInCheck(color: Color = Color.fromPlayer(currentPlayer)): Boolean = {
    val kingPosition = positions(ChessPiece(color, King, 0))
    isThreatened(kingPosition, color)
  }

  def listMovesOf(piece: ChessPiece): Seq[ChessMove] = {

    def withinBoard(x: Int) = x >= 0 && x <= 7

    def moveInDirection(piece: ChessPiece, init: Position, dx: Int, dy: Int): Seq[ChessMove] = {

      val Position(x, y) = init

      val emptyPositions: Seq[Position] = Stream.from(1).map(m => Position(x + m * dx, y + m * dy)).takeWhile { p =>
        withinBoard(p.row) && withinBoard(p.column) && board(p.row)(p.column).isEmpty
      }

      val nextM = emptyPositions.length + 1
      val nextX = x + nextM * dx
      val nextY = y + nextM * dy

      val positions = if(withinBoard(nextX) && withinBoard(nextY) && board(nextX)(nextY).get.color != piece.color) {
        emptyPositions :+ Position(nextX, nextY)
      } else {
        emptyPositions
      }

      positions.map(RegularChessMove(init, _))
    }

    val moves = Seq.newBuilder[ChessMove]
    val color = piece.color
    val position = positions(piece)

    piece.pieceType match {

      case Pawn => {
        val direction = if(color == White) 1 else -1
        val endRow = if(color == White) 7 else 0
        if(withinBoard(position.row + direction) && board(position.row + direction)(position.column).isEmpty) { // normal forward move
          if(position.row + direction == endRow) { // promotions
            moves ++= List(
              Promotion(position, Rook, Position(position.row + direction, position.column)),
              Promotion(position, Knight, Position(position.row + direction, position.column)),
              Promotion(position, Bishop, Position(position.row + direction, position.column)),
              Promotion(position, Queen, Position(position.row + direction, position.column))
            )
          } else {
            moves += RegularChessMove(position, Position(position.row + direction, position.column))
          }
        }
        val initRow = if(color == White) 1 else 6
        if(position.row == initRow) { // initial double speed forward move
          if(board(initRow + direction)(position.column).isEmpty && board(initRow + 2 * direction)(position.column).isEmpty) {
            moves += RegularChessMove(position, Position(position.row + 2 * direction, position.column))
          }
        }
        if(withinBoard(position.row + direction)) { // capture move
          for(shift <- -1 to 1 by 2) {
            if(withinBoard(position.column + shift)
              && board(position.row + direction)(position.column + shift).exists(_.color != color)) {
              if(position.row + direction == endRow) { // promotions
                moves ++= List(
                  Promotion(position, Rook, Position(position.row + direction, position.column + shift)),
                  Promotion(position, Knight, Position(position.row + direction, position.column + shift)),
                  Promotion(position, Bishop, Position(position.row + direction, position.column + shift)),
                  Promotion(position, Queen, Position(position.row + direction, position.column + shift))
                )
              } else {
                moves += RegularChessMove(position, Position(position.row + direction, position.column + shift))
              }
            }
          }
        }
      }

      case Rook => {
        for(dir <- -1 to 1 by 2) {
          moves ++= moveInDirection(piece, position, dir, 0)
          moves ++= moveInDirection(piece, position, 0, dir)
        }
      }

      case Knight => {
        for(dx <- -2 to 2 if dx != 0) {
          val abs_dy = 3 - abs(dx)
          for(dir <- -1 to 1 by 2) {
            val dy = dir * abs_dy
            if(withinBoard(position.row + dx) && withinBoard(position.column + dy)) {
              val destPiece = board(position.row + dx)(position.column + dy)
              if(destPiece.forall(_.color != color)) {
                moves += RegularChessMove(position, Position(position.row + dx, position.column + dy))
              }
            }
          }
        }
      }

      case Bishop => {
        for(dir_x <- -1 to 1 by 2) {
          for(dir_y <- -1 to 1 by 2) {
            moves ++= moveInDirection(piece, position, dir_x, dir_y)
          }
        }
      }

      case Queen => {
        for(dir <- -1 to 1 by 2) {
          moves ++= moveInDirection(piece, position, dir, 0)
          moves ++= moveInDirection(piece, position, 0, dir)
        }
        for(dir_x <- -1 to 1 by 2) {
          for(dir_y <- -1 to 1 by 2) {
            moves ++= moveInDirection(piece, position, dir_x, dir_y)
          }
        }
      }

      case King => {
        for(dx <- -1 to 1) {
          for(dy <- -1 to 1 by 2) {
            if(withinBoard(position.row + dx) && withinBoard(position.column + dy)) {
              val destPiece = board(position.row + dx)(position.column + dy)
              if(destPiece.forall(_.color != color)) {
                moves += RegularChessMove(position, Position(position.row + dx, position.column + dy))
              }
            }
          }
        }
        for(dx <- -1 to 1 by 2) {
          if(withinBoard(position.row + dx)) {
            val destPiece = board(position.row + dx)(position.column)
            if(destPiece.forall(_.color != color)) {
              moves += RegularChessMove(position, Position(position.row + dx, position.column))
            }
          }
        }
      }
    }

    moves.result
  }

  def possibleMoves: Seq[ChessMove] = {

    val color = Color.fromPlayer(currentPlayer)
    val moves: Seq[ChessMove] = positions.keys.filter(_.color == color).flatMap(listMovesOf).toSeq
    val specialMoves = Seq.newBuilder[ChessMove]

    // castling
    val row = if(color == White) 0 else 7
    val color_code = if(color == White) 0 else 1
    if(castlingRights(color)(0) && (1 to 3).forall(board(row)(_).isEmpty)) {
      if(!isInCheck(color) && !isThreatened(Position(row, 3), color)) {
        specialMoves += Castling(Position(row, 4), Position(row, 0))
      }
    }
    if(castlingRights(color)(1) && (5 to 6).forall(board(row)(_).isEmpty)) {
      if(!isInCheck(color) && !isThreatened(Position(row, 5), color)) {
        specialMoves += Castling(Position(row, 4), Position(row, 7))
      }
    }

    // en passant
    enPassantPosition.foreach { case Position(row, col) =>
      val dir = if(color == White) 1 else -1
      for(shift <- -1 to 1 by 2) {
        if(col + shift >= 0 && col + shift <= 7) {
          board(row - dir)(col + shift) match {
            case Some(p) if p.pieceType == Pawn && p.color == color =>
              specialMoves += EnPassant(Position(row - dir, col + shift), Position(row, col))
            case _ => ()
          }
        }
      }
    }

    (moves ++ specialMoves.result()).filter(!apply(_).isInCheck(color))
  }

  def evaluate: Evaluation = {
    if(possibleMoves.isEmpty) {
      if(isInCheck(Color.fromPlayer(currentPlayer))) Finished(-1) else Finished(0)
    } else {
      Playing
    }
  }

  override def toString: String = {

    def pieceLetter(p: PieceType): String = p match {
      case Pawn => "p"
      case Rook => "r"
      case Knight => "n"
      case Bishop => "b"
      case Queen => "q"
      case King => "k"
    }

    val representation: mutable.StringBuilder = new mutable.StringBuilder()

    val color = Color.fromPlayer(currentPlayer)
    representation ++= "  a b c d e f g h \n"
    for(y <- 7 to 0 by -1) {
      representation ++= s" +-+-+-+-+-+-+-+-+\n "
      for(x <- 0 to 7) {
        val pieceOption = board(y)(x)
        representation ++= "|"
        pieceOption match {
          case None => representation ++= " "
          case Some(piece) =>
            val code = pieceLetter(piece.pieceType)
            if (piece.color == White) representation ++= code.capitalize
            else representation ++= code
        }
      }
      representation ++= s"|${y + 1}\n"
    }
    representation ++= " +-+-+-+-+-+-+-+-+   " + color + " to play\n"
    representation ++= "  a b c d e f g h "

    representation.result()
  }

}

object OptimizedChessState {

  def apply(): OptimizedChessState = parseFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

  def parseFen(code: String): OptimizedChessState = {

    val pattern: Regex = """([a-h])([1-8])""".r

    def decodeAlgebraicNotation(code: String): Option[Position] = code match {
      case pattern(col_str, row_str) => Some(Position(row_str.charAt(0) - '1', col_str.charAt(0) - 'a'))
      case _ => None
    }

    def encodeAlgebraicNotation(position: Position): String = ('a' + position.column).toChar.toString + (1 + position.row).toString

    def addPiece(board: Array[Array[Option[ChessPiece]]], piece: ChessPiece, pos: Position): Unit = {
      board(pos.row)(pos.column) = Some(piece)
    }

    val words = code.split(" ")
    if(words.length != 6) throw new IllegalArgumentException("Fen: wrong number of fields")

    val castlingRights: MutableMap[Color, MutableIndexedSeq[Boolean]] = MutableMap(
      White -> MutableIndexedSeq.fill(2)(false),
      Black -> MutableIndexedSeq.fill(2)(false)
    )
    val board = Array.fill(8)(Array.fill[Option[ChessPiece]](8)(None))

    // parse board description
    var row = 7
    var col = 0
    val indices: MutableMap[Char, Int] = MutableMap().withDefaultValue(0)
    for(c <- words(0)) {
      c match {
        case '/' => {
          row -= 1
          col = 0
        }
        case _ if c - '0' >= 1 && c - '0' <= 8 => col += c - '0'
        case _ => {
          c match {
            case 'p' => addPiece(board, ChessPiece(Black, Pawn, indices(c)), Position(row, col))
            case 'r' => addPiece(board, ChessPiece(Black, Rook, indices(c)), Position(row, col))
            case 'n' => addPiece(board, ChessPiece(Black, Knight, indices(c)), Position(row, col))
            case 'b' => addPiece(board, ChessPiece(Black, Bishop, indices(c)), Position(row, col))
            case 'q' => addPiece(board, ChessPiece(Black, Queen, indices(c)), Position(row, col))
            case 'k' => addPiece(board, ChessPiece(Black, King, indices(c)), Position(row, col))
            case 'P' => addPiece(board, ChessPiece(White, Pawn, indices(c)), Position(row, col))
            case 'R' => addPiece(board, ChessPiece(White, Rook, indices(c)), Position(row, col))
            case 'N' => addPiece(board, ChessPiece(White, Knight, indices(c)), Position(row, col))
            case 'B' => addPiece(board, ChessPiece(White, Bishop, indices(c)), Position(row, col))
            case 'Q' => addPiece(board, ChessPiece(White, Queen, indices(c)), Position(row, col))
            case 'K' => addPiece(board, ChessPiece(White, King, indices(c)), Position(row, col))
            case d => throw new IllegalArgumentException("Fen: did not understand board description: " + d)
          }
          indices(c) += 1
          col += 1
        }
      }
    }

    // parse castling rights
    for(c <- words(2)) {
      c match {
        case 'Q' => castlingRights(White)(0) = true
        case 'K' => castlingRights(White)(1) = true
        case 'q' => castlingRights(Black)(0) = true
        case 'k' => castlingRights(Black)(1) = true
        case '-' => ()
        case _ => throw new IllegalArgumentException("Fen: did not understand castling rights")
      }
    }

    // parse en passant marker
    val enPassantPosition = words(3) match {
      case "-" => None
      case s => decodeAlgebraicNotation(s)
    }

    // parse turn
    val turn = 2 * (words(5).toInt - 1) + ( if(words(1) == "b") 1 else 0 )

    val positions: Seq[(ChessPiece, Position)] = for(
      (seq, row) <- board.zipWithIndex;
      (pieceOption, column) <- board(row).zipWithIndex;
      piece <- pieceOption.toSeq
    ) yield piece -> Position(row, column)

    OptimizedChessState(
      turn % 2,
      board,
      ImmutableMap(positions.toSeq:_*),
      ImmutableMap(castlingRights.mapValues(row => IndexedSeq(row:_*)).toSeq:_*),
      enPassantPosition
    )
  }

}
