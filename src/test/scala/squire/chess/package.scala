package squire

import squire.base.State

import scala.io.BufferedSource

package object chess {

  def countMoves[S <: State[S]](state: State[S], depth: Int): Int = depth match {

    case 1 => {
      val moves = state.possibleMoves
      moves.length
    }

    case d => {
      val moves = state.possibleMoves
      moves.map(m => countMoves(state.apply(m), d - 1)).reduceLeft(_ + _)
    }
  }

  case class Perft(fen: String, depth: Int, moveCount: MoveCount)

  def readPerft(line: String): Perft = {
    val words = line.split(" ")
    val fen = words.slice(0, 6).reduceLeft(_ + " " + _)
    val depth = words(6).toInt
    val moveCount = MoveCount.fromSeq(words.slice(7, 13).map(_.toLong))
    Perft(fen, depth, moveCount)
  }

  def readPerftFile(source: BufferedSource): Iterator[Perft] = {
    for(
      line <- source.getLines if !line.startsWith("#")
    ) yield readPerft(line)
  }

}
