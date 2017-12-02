// current limitations:
// + three-fold repetion not implemented
// + fifty-move rule not implemented


package old

import scala.collection.mutable.ArrayBuffer
import scala.math.abs


object Color extends Enumeration {
    type Color = Value
    val Black, White = Value
}
import old.Color._


sealed abstract class ChessPiece(val color: Color)
case class Pawn(override val color: Color, id: Int) extends ChessPiece(color)
case class King(override val color: Color) extends ChessPiece(color)
case class Queen(override val color: Color, id: Int) extends ChessPiece(color)
case class Rook(override val color: Color, id: Int) extends ChessPiece(color)
case class Knight(override val color: Color, id: Int) extends ChessPiece(color)
case class Bishop(override val color: Color, id: Int) extends ChessPiece(color)


case class Position(val row: Int, val column: Int)


sealed abstract class ChessMove
case class RegularChessMove(val origin: Position, val destination: Position) extends ChessMove
case class Castling(val kingPos: Position, val rookPos: Position) extends ChessMove
case class Promotion(val origin: Position, val promoted: Char, val destination: Position) extends ChessMove
case class EnPassant(val origin: Position, val destination: Position) extends ChessMove


class ChessState(val turn: Int, val board: Array[Array[ChessPiece]], val positions: Map[ChessPiece, Position], val castlingRights: Array[Array[Boolean]], val enPassantPosition: Option[Position]) extends GameState[ChessMove] {

    def getPiece(pos: Position) = board(pos.row)(pos.column)

    override def apply[Move >: ChessMove](move: Move): ChessState = {

        // This method does not test if the move is a valid move.
        // The moves returned by the method possibleMoves() are all valid.
        // The tests done here only ensure the integrity of the pointers between
        // the board and the positions map.

    	val newBoard = board.map(_.clone)
        val newCastlingRights = castlingRights.map(_.clone)

        move match {

            case RegularChessMove(origin, destination) => {
                val piece = getPiece(origin)
                newBoard(origin.row)(origin.column) = null
                var deletedPiece = getPiece(destination)
                if(deletedPiece == King(White) || deletedPiece == King(Black)) throw new IllegalArgumentException("king deleted")
                newBoard(destination.row)(destination.column) = piece

                // handle castling rights
                val color = piece.color
                val color_code = if(color == White) 0 else 1
                piece match {
                    case King(_) => {
                        newCastlingRights(color_code)(0) = false
                        newCastlingRights(color_code)(1) = false
                    }
                    case Rook(_, _) => {
                        val row = if(color == White) 0 else 7
                        if(origin.row == row && origin.column == 0) newCastlingRights(color_code)(0) = false
                        if(origin.row == row && origin.column == 7) newCastlingRights(color_code)(1) = false
                    }
                    case _ => ()
                }

                // handle castling rights if a rook is captured
                val opp_row = if(color == Black) 0 else 7
                val opp_color_code = (color_code + 1) % 2
                if(destination.row == opp_row && destination.column == 0) {
                    deletedPiece match {
                        case _: Rook => newCastlingRights(opp_color_code)(0) = false
                        case _ => ()
                    }
                }
                if(destination.row == opp_row && destination.column == 7) {
                    deletedPiece match {
                        case _: Rook => newCastlingRights(opp_color_code)(1) = false
                        case _ => ()
                    }
                }

                // handle en passant marking
                val newEnPassantPosition = piece match {
                    case _: Pawn => if(abs(origin.row - destination.row) == 2) {
                        val dir = if(color == White) 1 else -1
                        Some(Position(destination.row - dir, destination.column))
                    } else None
                    case _ => None
                }

                deletedPiece = if(deletedPiece != null) deletedPiece else Pawn(White, -1) // fake piece to avoid if statement
                new ChessState(turn + 1, newBoard, positions + (piece -> destination) - deletedPiece, newCastlingRights, newEnPassantPosition)
            }

            case Castling(kingPos, rookPos) => {
                val king = getPiece(kingPos)
                val rook = getPiece(rookPos)
                if(king == null)  throw new IllegalArgumentException("castling: king is not where you said")
                if(rook == null)  throw new IllegalArgumentException("castling: there is no rook where you said")
                val row = if(king.color == White) 0 else 7
                val color_code = if(king.color == White) 0 else 1
                if(kingPos.row != row || kingPos.column != 4) throw new IllegalArgumentException("castling: king not in the right place")
                if(rookPos.row != row) throw new IllegalArgumentException("castling: rook not in the right row")
                if(rookPos.column == 0) {
                    newBoard(row)(4) = null
                    newBoard(row)(0) = null
                    if(board(row)(3) != null || board(row)(2) != null) throw new IllegalArgumentException("castling: some pieces between rook and king")
                    newBoard(row)(3) = rook
                    newBoard(row)(2) = king
                    newCastlingRights(color_code)(0) = false
                    newCastlingRights(color_code)(1) = false
                    new ChessState(turn + 1, newBoard, positions + (rook -> Position(row, 3)) + (king -> Position(row, 2)), newCastlingRights, None)
                } else if(rookPos.column == 7) {
                    newBoard(row)(4) = null
                    newBoard(row)(7) = null
                    if(board(row)(5) != null || board(row)(6) != null) throw new IllegalArgumentException("castling: some pieces between rook and king")
                    newBoard(row)(5) = rook
                    newBoard(row)(6) = king
                    newCastlingRights(color_code)(0) = false
                    newCastlingRights(color_code)(1) = false
                    new ChessState(turn + 1, newBoard, positions + (rook -> Position(row, 5)) + (king -> Position(row, 6)), newCastlingRights, None)
                } else throw new IllegalArgumentException
            }

            case Promotion(origin, promoted, destination) => {
                val pawn = getPiece(origin)
                pawn match {
                    case _: Pawn => ()
                    case _ => throw new IllegalArgumentException("promotion: piece is not a pawn")
                }
                newBoard(origin.row)(origin.column) = null
                var deletedPiece = board(destination.row)(destination.column)
                val newPiece = promoted match {
                    case 'r' => Rook(pawn.color, turn + 2) // just to be sure in case of perft
                    case 'n' => Knight(pawn.color, turn + 2)
                    case 'b' => Bishop(pawn.color, turn + 2)
                    case _ => Queen(pawn.color, turn + 2)
                }
                newBoard(destination.row)(destination.column) = newPiece
                deletedPiece = if(deletedPiece != null) deletedPiece else Pawn(White, -1) // fake piece to avoid if statement
                new ChessState(turn + 1, newBoard, positions + (newPiece -> destination) - pawn - deletedPiece, newCastlingRights, None)
            }

            case EnPassant(origin, destination) => {
                val piece = getPiece(origin)
                newBoard(origin.row)(origin.column) = null
                val deletedPiece = getPiece(Position(origin.row, destination.column))
                if(deletedPiece == null) throw new IllegalArgumentException("en passant: there is no piece deleted in this en passant move")
                newBoard(destination.row)(destination.column) = piece
                newBoard(origin.row)(destination.column) = null
                new ChessState(turn + 1, newBoard, positions + (piece -> destination) - deletedPiece, newCastlingRights, None)
            }
        }
    }

    def threatens(piece: ChessPiece, destination: Position) = {
    	val pos = positions(piece)
        val dx = destination.row - pos.row
        val dy = destination.column - pos.column
        val dir_x = if(dx >= 0) 1 else -1
        val dir_y = if(dy >= 0) 1 else -1
    	piece match {
	    	case Pawn(color, _) => {
	    		val direction = color match {
	    			case White => 1
	    			case Black => -1
	    		}
	    		(dx == direction) && (abs(dy) == 1)
	    		// en passant is missing
	    	}
	    	case Rook(_, _) => {
	    		if(dx != 0 && dy != 0) false
	    		else if(dy == 0) {
	    			if(abs(dx) <= 1) true
	    			else (1 until abs(dx)).map(d => board(pos.row + dir_x * d)(pos.column) == null).reduceLeft(_ && _)
	    		} else {
	    			if(abs(dy) <= 1) true
	    			else (1 until abs(dy)).map(d => board(pos.row)(pos.column + dir_y * d) == null).reduceLeft(_ && _)
	    		}
	    	}
	    	case Knight(_, _) => (abs(dx) == 2 && abs(dy) == 1) || (abs(dx) == 1 && abs(dy) == 2)
            case Bishop(_, _) => {
                if(abs(dx) != abs(dy)) false
                else if(abs(dx) <= 1) true
                else {
                    (1 until abs(dx)).map(d => board(pos.row + dir_x * d)(pos.column + dir_y * d) == null).reduceLeft(_ && _)
                }
            }
            case Queen(_, _) => {
                if(dy == 0) {
                    if(abs(dx) <= 1) true
                    else (1 until abs(dx)).map(d => board(pos.row + dir_x * d)(pos.column) == null).reduceLeft(_ && _)
                } else if(dx == 0) {
                    if(abs(dy) <= 1) true
                    else (1 until abs(dy)).map(d => board(pos.row)(pos.column + dir_y * d) == null).reduceLeft(_ && _)
                } else if(abs(dx) == abs(dy)) {
                    if(abs(dx) <= 1) true
                    else {
                        (1 until abs(dx)).map(d => board(pos.row + dir_x * d)(pos.column + dir_y * d) == null).reduceLeft(_ && _)
                    }
                } else false
            }
            case King(_) => (abs(dx) <= 1 && abs(dy) <= 1)
    	}
    }

    def isThreatened(position: Position, color: Color): Boolean = {

        def withinBoard(x: Int) = x >= 0 && x <= 7

        // pawns
        val dir = if(color == White) 1 else -1
        if(withinBoard(position.row + dir)) {
            for(dx  <- -1 to 1 by 2) {
                if(withinBoard(position.column + dx)) {
                    val piece = board(position.row + dir)(position.column + dx)
                    piece match {
                        case Pawn(c, _) => if(c != color) return true
                        case _ => ()
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
                    val piece = board(position.row + dx)(position.column + dy)
                    if(piece != null) {
                        piece match {
                            case Knight(c, _) => if(c != color) return true
                            case _ => ()
                        }
                    }
                }
            }
        }

        // the rest
        def moveInDirection(dx: Int, dy: Int) = {
            var m = 1
            val Position(x, y) = position
            while(withinBoard(x + m * dx) && withinBoard(y + m * dy) && board(x + m * dx)(y + m * dy) == null) {
                m += 1
            }
            if(withinBoard(x + m * dx) && withinBoard(y + m * dy) && board(x + m * dx)(y + m * dy).color != color) {
                val piece = board(x + m * dx)(y + m * dy)
                if(dx == 0 || dy == 0) {
                    piece match {
                        case _: King => (m == 1)
                        case _: Queen => true
                        case _: Rook => true
                        case _ => false
                    }
                } else {
                    piece match {
                        case _: King => (m == 1)
                        case _: Queen => true
                        case _: Bishop => true
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

        return false
    }

    def isInCheck(color: Color) = {
        val king_pos = positions(King(color))
        isThreatened(king_pos, color)
    }

    def listMovesOf(piece: ChessPiece): List[ChessMove] = {

        def withinBoard(x: Int) = x >= 0 && x <= 7

        def moveInDirection(piece: ChessPiece, init: Position, dx: Int, dy: Int) = {
            var output: List[ChessMove] = List()
            var m = 1
            val Position(x, y) = init
            while(withinBoard(x + m * dx) && withinBoard(y + m * dy) && board(x + m * dx)(y + m * dy) == null) {
                output ::= RegularChessMove(init, Position(x + m * dx, y + m * dy))
                m += 1
            }
            if(withinBoard(x + m * dx) && withinBoard(y + m * dy) && board(x + m * dx)(y + m * dy).color != piece.color) {
                output ::= RegularChessMove(init, Position(x + m * dx, y + m * dy))
            }
            output
        }

        var output: List[ChessMove] = List()
        val color = piece.color
        val position = positions(piece)

        piece match {

            case Pawn(c, n) => {
                val direction = if(color == White) 1 else -1
                val end_row = if(color == White) 7 else 0
                if(withinBoard(position.row + direction) && board(position.row + direction)(position.column) == null) { // normal forward move
                    if(position.row + direction == end_row) { // promotions
                        output :::= List(
                            Promotion(position, 'r', Position(position.row + direction, position.column)),
                            Promotion(position, 'n', Position(position.row + direction, position.column)),
                            Promotion(position, 'b', Position(position.row + direction, position.column)),
                            Promotion(position, 'q', Position(position.row + direction, position.column))
                        )
                    } else output ::= RegularChessMove(position, Position(position.row + direction, position.column))
                }
                val init_row = if(color == White) 1 else 6
                if(position.row == init_row) { // initial double speed forward move
                    if(board(init_row + direction)(position.column) == null && board(init_row + 2 * direction)(position.column) == null) {
                        output ::= RegularChessMove(position, Position(position.row + 2 * direction, position.column))
                    }
                }
                if(withinBoard(position.row + direction)) { // capture move
                    for(shift <- -1 to 1 by 2) {
                        if(withinBoard(position.column + shift)
                           && board(position.row + direction)(position.column + shift) != null
                           && board(position.row + direction)(position.column + shift).color != color) {
                            if(position.row + direction == end_row) { // promotions
                                output :::= List(
                                    Promotion(position, 'r', Position(position.row + direction, position.column + shift)),
                                    Promotion(position, 'n', Position(position.row + direction, position.column + shift)),
                                    Promotion(position, 'b', Position(position.row + direction, position.column + shift)),
                                    Promotion(position, 'q', Position(position.row + direction, position.column + shift))
                                )
                            } else output ::= RegularChessMove(position, Position(position.row + direction, position.column + shift))
                        }
                    }
                }
            }

            case Rook(_, _) => {
                for(dir <- -1 to 1 by 2) {
                    output :::= moveInDirection(piece, position, dir, 0)
                    output :::= moveInDirection(piece, position, 0, dir)
                }
            }

            case Knight(_, _) => {
                for(dx <- -2 to 2 if dx != 0) {
                    val abs_dy = 3 - abs(dx)
                    for(dir <- -1 to 1 by 2) {
                        val dy = dir * abs_dy
                        if(withinBoard(position.row + dx) && withinBoard(position.column + dy)) {
                            val destPiece = board(position.row + dx)(position.column + dy)
                            if(destPiece == null || destPiece.color != color) {
                                output ::= RegularChessMove(position, Position(position.row + dx, position.column + dy))
                            }
                        }
                    }
                }
            }

            case Bishop(_, _) => {
                for(dir_x <- -1 to 1 by 2) {
                    for(dir_y <- -1 to 1 by 2) {
                        output :::= moveInDirection(piece, position, dir_x, dir_y)
                    }
                }
            }

            case Queen(_, _) => {
                for(dir <- -1 to 1 by 2) {
                    output :::= moveInDirection(piece, position, dir, 0)
                    output :::= moveInDirection(piece, position, 0, dir)
                }
                for(dir_x <- -1 to 1 by 2) {
                    for(dir_y <- -1 to 1 by 2) {
                        output :::= moveInDirection(piece, position, dir_x, dir_y)
                    }
                }
            }

            case King(_) => {
                for(dx <- -1 to 1) {
                    for(dy <- -1 to 1 by 2) {
                        if(withinBoard(position.row + dx) && withinBoard(position.column + dy)) {
                            val destPiece = board(position.row + dx)(position.column + dy)
                            if(destPiece == null || destPiece.color != color) {
                                output ::= RegularChessMove(position, Position(position.row + dx, position.column + dy))
                            }
                        }
                    }
                }
                for(dx <- -1 to 1 by 2) {
                    if(withinBoard(position.row + dx)) {
                        val destPiece = board(position.row + dx)(position.column)
                        if(destPiece == null || destPiece.color != color) {
                            output ::= RegularChessMove(position, Position(position.row + dx, position.column))
                        }
                    }
                }
            }
        }

        output

    }

    def possibleMoves(): List[ChessMove] = {

        val color = if(turn % 2 == 0) White else Black
        var moves = positions.keys.filter(_.color == color).flatMap(listMovesOf(_)).toList

        // castling
        val row = if(color == White) 0 else 7
        val color_code = if(color == White) 0 else 1
        if(castlingRights(color_code)(0) && (1 to 3).map(board(row)(_) == null).reduceLeft(_ && _)) {
            if(!isInCheck(color) && !isThreatened(Position(row, 3), color)) {
                moves ::= Castling(Position(row, 4), Position(row, 0))
            }
        }
        if(castlingRights(color_code)(1) && (5 to 6).map(board(row)(_) == null).reduceLeft(_ && _)) {
            if(!isInCheck(color) && !isThreatened(Position(row, 5), color)) {
                moves ::= Castling(Position(row, 4), Position(row, 7))
            }
        }

        // en passant
        enPassantPosition match {
            case Some(Position(row, col)) => {
                val dir = if(color == White) 1 else -1
                for(shift <- -1 to 1 by 2) {
                    if(col + shift >= 0 && col + shift <= 7) {
                        val piece = board(row - dir)(col + shift)
                        piece match {
                            case Pawn(c, _) if c == color => {
                                moves ::= EnPassant(Position(row - dir, col + shift), Position(row, col))
                            }
                            case _ => ()
                        }
                    }
                }
            }
            case None => ()
        }

        moves.filter(!apply(_).isInCheck(color))
    }

    def evaluate(): Option[Int] = {
        val color = if(turn % 2 == 0) White else Black
        val opponent_code = if(color == White) -1 else 1
        if(possibleMoves.length == 0) {
            if(isInCheck(color)) Some(opponent_code) else Some(0)
        } else None
    }

    override def toString(): String = {
        val color = if(turn % 2 == 0) White else Black
        var representation = "  a b c d e f g h \n"
        for(y <- 7 to 0 by -1) {
            representation += " +-+-+-+-+-+-+-+-+\n" + (y + 1).toString
            for(x <- 0 to 7) {
                val piece = board(y)(x)
                representation += "|"
                if(piece == null) representation += " "
                else {
                    val code = piece match {
                        case Pawn(_, _) => "p"
                        case Rook(_, _) => "r"
                        case Knight(_, _) => "n"
                        case Bishop(_, _) => "b"
                        case Queen(_, _) => "q"
                        case King(_) => "k"
                    }
                    if(piece.color == White) representation += code.capitalize
                    else representation += code
                }
            }
            representation += "|" + (y + 1).toString + "\n"
        }
        representation += " +-+-+-+-+-+-+-+-+   " + color + " to play\n"
        representation += "  a b c d e f g h "

        representation
    }

    override def features(): Array[Feature] = {

        implicit def bool2int(b:Boolean) = if (b) 1 else 0

        val n_features = 100
        val features: ArrayBuffer[Feature] = new ArrayBuffer
        for(color_code <- 0 to 1) {
            val color = if(color_code == 0) White else Black

            features += Feature(color.toString + " in check", isInCheck(color):Int)

            features += Feature("# " + color.toString + " pawns",
                                positions.keysIterator.map(piece => piece match {
                                    case Pawn(c, _) if c == color => 1
                                    case _ => 0
                                }).reduceLeft(_ + _))

            features += Feature("# " + color.toString + " rooks",
                                positions.keysIterator.map(piece => piece match {
                                    case Rook(c, _) if c == color => 1
                                    case _ => 0
                                }).reduceLeft(_ + _))

            features += Feature("# " + color.toString + " knights",
                                positions.keysIterator.map(piece => piece match {
                                    case Knight(c, _) if c == color => 1
                                    case _ => 0
                                }).reduceLeft(_ + _))

            features += Feature("# " + color.toString + " bishops",
                                positions.keysIterator.map(piece => piece match {
                                    case Bishop(c, _) if c == color => 1
                                    case _ => 0
                                }).reduceLeft(_ + _))

            features += Feature("# " + color.toString + " queens",
                                positions.keysIterator.map(piece => piece match {
                                    case Queen(c, _) if c == color => 1
                                    case _ => 0
                                }).reduceLeft(_ + _))

            for(i <- 0 to 1) {
                val order_name = if(i == 0) "first" else "second"
                val pieces = Map("rook" -> Rook(color, i),
                                 "knight" -> Knight(color, i),
                                 "bishop" -> Bishop(color, i),
                                 "queen" -> Queen(color, i) // we use two queens because it is usual to obtain a second queen
                                )

                for((name, piece) <- pieces) {
                    features += Feature("# pieces threatening " + order_name + " " + color.toString + " " + name,
                        if(positions.contains(piece)) {
                            positions.keysIterator.filter(_.color != color).map(threatens(_, positions(piece)):Int).reduceLeft(_ + _)
                        } else 0)
                    features += Feature("# pieces defending " + order_name + " " + color.toString + " " + name,
                        if(positions.contains(piece)) {
                            positions.keysIterator.filter(_.color == color).filter(_ != piece).map(threatens(_, positions(piece)):Int).reduceLeft(_ + _)
                        } else 0)
                }
            }
        }

        features.toArray
    }

    def pieceCode(piece: ChessPiece) = piece match {
        case Pawn(White, _) => 0
        case Pawn(Black, _) => 1
        case Rook(White, _) => 2
        case Rook(Black, _) => 3
        case Knight(White, _) => 4
        case Knight(Black, _) => 5
        case Bishop(White, _) => 6
        case Bishop(Black, _) => 7
        case Queen(White, _) => 8
        case Queen(Black, _) => 9
        case King(White) => 10
        case King(Black) => 11
        case _ => 11
    }

    override def hashCode: Int = {
        var h = (turn % 2) * ChessState.turn_hash
        for(i <- 0 until 2) {
            for(j <- 0 until 2) {
                if(castlingRights(i)(j)) h = h ^ ChessState.castling_hash(i)(j)
            }
        }
        enPassantPosition match {
            case Some(Position(row, col)) => {
                h = h ^ ChessState.enpassant_hash(if(row == 2) 0 else 1)(col)
            }
            case None => ()
        }
        for(i <- 0 until 8) {
            for(j <- 0 until 8) {
                val piece = board(i)(j)
                if(piece != null) h = h ^ ChessState.board_hash(i * 8 + j)(pieceCode(piece))
            }
        }
        return h
    }

    override def equals(that: Any): Boolean = that match {
        case that: ChessState => this.hashCode == that.hashCode
        case _ => false
    }

    def decodeANMove(code: String): Option[ChessMove] = code.length match {
        case 4 => { // anything but promotion
            val srcOption = ChessState.decodeAlgebraicNotation(code.slice(0,2))
            val destOption = ChessState.decodeAlgebraicNotation(code.slice(2,4))
            if(srcOption == None || destOption == None) return None
            val src = srcOption.get
            val dest = destOption.get

            val d_row = dest.row - src.row
            val d_col = dest.column - src.column
            val piece = getPiece(src)

            // castling
            if((piece == King(White) || piece == King(Black)) && abs(d_col) == 2) {
                return Some(Castling(src, Position(dest.row, if(d_col > 0) 7 else 0)))
            }

            // en passant
            piece match {
                case Pawn(_, _) => if(abs(d_col) > 0 && getPiece(dest) == null) return Some(EnPassant(src, dest))
                case _ => ()
            }

            return Some(RegularChessMove(src, dest))
        }
        case 5 => { // promotion
            val src = ChessState.decodeAlgebraicNotation(code.slice(0,2))
            val dest = ChessState.decodeAlgebraicNotation(code.slice(2,4))
            if(src != None && dest != None) Some(Promotion(src.get, code(4), dest.get))
            else None
        }
        case _ => None
    }

    def encodeANMove(move: ChessMove): String = move match {
        case RegularChessMove(src, dest) => ChessState.encodeAlgebraicNotation(src) + ChessState.encodeAlgebraicNotation(dest)
        case Promotion(src, promoted, dest) => ChessState.encodeAlgebraicNotation(src) + ChessState.encodeAlgebraicNotation(dest) + promoted
        case Castling(kingPos, rookPos) => if(rookPos.column > kingPos.column)
                ChessState.encodeAlgebraicNotation(kingPos) + ChessState.encodeAlgebraicNotation(Position(kingPos.row, kingPos.column + 2))
            else
                ChessState.encodeAlgebraicNotation(kingPos) + ChessState.encodeAlgebraicNotation(Position(kingPos.row, kingPos.column - 2))
        case EnPassant(src, dest) => ChessState.encodeAlgebraicNotation(src) + ChessState.encodeAlgebraicNotation(dest)
    }
}


object ChessState {

    // companion object defining initial constructor and hash function values

    val board_hash = Array.ofDim[Int](64, 12)
    for(i <- 0 until 64) {
        for(j <- 0 until 12) {
            board_hash(i)(j) = scala.util.Random.nextInt
        }
    }
    val castling_hash = Array.ofDim[Int](2, 2)
    for(i <- 0 until 2) {
        for(j <- 0 until 2) {
            castling_hash(i)(j) = scala.util.Random.nextInt
        }
    }
    val enpassant_hash = Array.ofDim[Int](2, 8)
    for(i <- 0 until 2) {
        for(j <- 0 until 8) {
            enpassant_hash(i)(j) = scala.util.Random.nextInt
        }
    }
    val turn_hash = scala.util.Random.nextInt

	def addPiece(board: Array[Array[ChessPiece]], positions: Map[ChessPiece, Position], piece: ChessPiece, pos: Position) = {
    	board(pos.row)(pos.column) = piece
    	positions + (piece -> pos)
    }

	def apply() = parseFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

    val pattern = """([a-h])([1-8])""".r

    def decodeAlgebraicNotation(code: String): Option[Position] = code match {
        case pattern(col_str, row_str) => Some(Position((row_str.charAt(0) - '1'):Int, (col_str.charAt(0) - 'a'):Int))
        case _ => None
    }

    def encodeAlgebraicNotation(position: Position): String = ('a' + position.column).toChar.toString + (1 + position.row).toString

    def parseFen(code: String) = {

        implicit def char2str(c: Char) = c.toString

        val words = code.split(" ")
        if(words.length != 6) throw new IllegalArgumentException("Fen: wrong number of fields")

        val castlingRights = Array.fill[Boolean](2, 2)(false)
        val board = Array.ofDim[ChessPiece](8, 8)
        var positions: Map[ChessPiece, Position] = Map()

        // parse board description
        var row = 7
        var col = 0
        var indices: scala.collection.mutable.Map[Char, Int] = scala.collection.mutable.Map().withDefaultValue(0)
        for(c <- words(0)) {
            c match {
                case '/' => {
                    row -= 1
                    col = 0
                }
                case c if (c - '0' >= 1 && c - '0' <= 8) => col += c - '0'
                case _ => {
                    c match {
                        case 'p' => positions = addPiece(board, positions, Pawn(Black, indices(c)), Position(row, col))
                        case 'r' => positions = addPiece(board, positions, Rook(Black, indices(c)), Position(row, col))
                        case 'n' => positions = addPiece(board, positions, Knight(Black, indices(c)), Position(row, col))
                        case 'b' => positions = addPiece(board, positions, Bishop(Black, indices(c)), Position(row, col))
                        case 'q' => positions = addPiece(board, positions, Queen(Black, indices(c)), Position(row, col))
                        case 'k' => positions = addPiece(board, positions, King(Black), Position(row, col))
                        case 'P' => positions = addPiece(board, positions, Pawn(White, indices(c)), Position(row, col))
                        case 'R' => positions = addPiece(board, positions, Rook(White, indices(c)), Position(row, col))
                        case 'N' => positions = addPiece(board, positions, Knight(White, indices(c)), Position(row, col))
                        case 'B' => positions = addPiece(board, positions, Bishop(White, indices(c)), Position(row, col))
                        case 'Q' => positions = addPiece(board, positions, Queen(White, indices(c)), Position(row, col))
                        case 'K' => positions = addPiece(board, positions, King(White), Position(row, col))
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
                case 'Q' => castlingRights(0)(0) = true
                case 'K' => castlingRights(0)(1) = true
                case 'q' => castlingRights(1)(0) = true
                case 'k' => castlingRights(1)(1) = true
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

        new ChessState(turn, board, positions, castlingRights, enPassantPosition)
    }
}
