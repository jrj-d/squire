package squire.chess

import squire.base.State
import squire.utils.Tabulator

import scala.collection.immutable.ListMap
import scala.io.{BufferedSource, Source}

object Performance {

  def time[R](block: => R): (R, Double) = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    (result, (t1 - t0) / 1e6)
  }

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

  case class Perft(fen: String, depth: Int)

  case class Result(nMoves: Int, duration: Double)

  def evaluatePerft(perft: Perft, stateCreator: String => (State[S] forSome {type S <: State[S]})): Result = {
    val initState = stateCreator(perft.fen)
    val (nLeaves, duration) = time(countMoves(initState, perft.depth))
    Result(nLeaves, duration)
  }

  def readFen(line: String): Perft = {
    val words = line.split(" ")
    val fen = words.slice(0, 6).reduceLeft(_ + " " + _)
    val depth = words(6).toInt
    Perft(fen, depth)
  }

  def readPerftFile(source: BufferedSource): Iterator[Perft] = {
    for(
      line <- source.getLines if !line.startsWith("#")
    ) yield readFen(line)
  }

  def runBenchmark(perfts: Seq[Perft], programs: ListMap[String, String => (State[S] forSome {type S <: State[S]})]): Unit = {

    val results: List[List[Result]] = for (
      perft <- perfts.toList
    ) yield {
      for(
        program <- programs.values.toList
      ) yield evaluatePerft(perft, program)
    }

    println("")
    println("Number of leaves")
    println("====================================")
    println("")

    val leaves: String = Tabulator.format(
      ("Perft" +: programs.keys.toSeq) +:
        perfts.zip(results).map { case (perft, row) =>
          s"${perft.fen} at depth ${perft.depth}" +: row.map(_.nMoves)
        }
    )
    println(leaves)

    println("")
    println("Duration in ms")
    println("====================================")
    println("")

    val durations: String = Tabulator.format(
      ("Perft" +: programs.keys.toSeq) +:
      perfts.zip(results).map { case (perft, row) =>
        s"${perft.fen} at depth ${perft.depth}" +: row.map(_.duration)
      }.toList
    )
    println(durations)

  }

  def main(args: Array[String]): Unit = {

    val source: BufferedSource = Source.fromURL(getClass.getResource("/perfts.fen"))
    val perfts: Seq[Perft] = readPerftFile(source).toList

    val programs = ListMap[String, String => (State[S] forSome {type S <: State[S]})](

      "initial revamp" -> ((s: String) => ChessState.parseFen(s)),

      /*"initial revamp with threatens" -> { (s: String) =>
        val originalState = ChessState.parseFen(s)
        new ChessState(originalState.currentPlayer, originalState.board, originalState.positions, originalState.castlingRights, originalState.enPassantPosition) {
          override def isInCheck(color: Color): Boolean = {
            val kingPosition = positions(ChessPiece(color, King, 0))
            positions.keys.filter(_.color != color).map(threatens(_, kingPosition)).reduceLeft(_ || _)
          }
        }
      },*/

      "optimized revamp" -> ((s: String) => OptimizedChessState.parseFen(s))
    )

    runBenchmark(perfts, programs)
  }
}
