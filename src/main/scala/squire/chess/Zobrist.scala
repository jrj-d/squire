package squire.chess

import scala.util.Random

// scalastyle:off magic.number

class Zobrist(seed: Long) {

  val generator = new Random(seed)

  val boardHash: IndexedSeq[IndexedSeq[IndexedSeq[Int]]] = IndexedSeq.fill(8)(
    IndexedSeq.fill(8)(
      IndexedSeq.fill(12)(
        generator.nextInt
      )
    )
  )

  val castlingHash: IndexedSeq[IndexedSeq[Int]] = IndexedSeq.fill(2)(
    IndexedSeq.fill(2)(
      generator.nextInt
    )
  )

  val enPassantHash: IndexedSeq[IndexedSeq[Int]] = IndexedSeq.fill(2)(
    IndexedSeq.fill(8)(
      generator.nextInt
    )
  )

  val currentPlayerHash: Int = generator.nextInt

  def pieceCode(piece: ChessPiece): Int = {
    val base = piece.pieceType match {
      case Pawn => 0
      case Rook => 2
      case Knight => 4
      case Bishop => 6
      case Queen => 8
      case King => 10
    }
    base + piece.color.id
  }

  def hash(state: ChessState): Int = {

    var h: Int = 0

    if (state.currentPlayer == 1) {
      h = h ^ currentPlayerHash
    }

    for(i <- 0 until 2) {
      for(j <- 0 until 2) {
        if(state.castlingRights(i)(j)) {
          h = h ^ castlingHash(i)(j)
        }
      }
    }

    state.enPassantPosition.foreach { case Position(row, col) =>
      val index = if(row == 2) 0 else 1
      h = h ^ enPassantHash(index)(col)
    }

    for(i <- 0 until 8) {
      for(j <- 0 until 8) {
        state.board(i)(j).foreach { piece =>
          h = h ^ boardHash(i)(j)(pieceCode(piece))
        }
      }
    }

    h
  }

}

object Zobrist extends Zobrist(42)
