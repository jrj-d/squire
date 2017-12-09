package squire.chess

import squire.base.State
import squire.utils.{time, Tabulator}

import scala.collection.immutable.ListMap
import scala.io.{BufferedSource, Source}

object Performance {

  case class Result(nMoves: Int, duration: Double)

  def evaluatePerft(perft: Perft, stateCreator: String => (State[S] forSome {type S <: State[S]})): Result = {
    val initState = stateCreator(perft.fen)
    val (nLeaves, duration) = time(countMoves(initState, perft.depth))
    Result(nLeaves, duration)
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

    val source: BufferedSource = Source.fromURL(getClass.getResource("/perfts-for-rules.fen"))
    val perfts: Seq[Perft] = readPerftFile(source).toList

    val programs = ListMap[String, String => (State[S] forSome {type S <: State[S]})](

      "optimized revamp" -> ((s: String) => ChessState.parseFen(s)),

      "optimized revamp with threatens" -> { (s: String) =>
        val originalState = ChessState.parseFen(s)
        new ChessState(
          originalState.ply, originalState.halfMoveClock, originalState.board, originalState.positions,
          originalState.castlingRights, originalState.enPassantPosition,
          Map.empty[Int, Int]
        ) {
          override def isInCheck(color: Color): Boolean = {
            val kingPosition = positions(ChessPiece(color, King, 0))
            positions.keys.filter(_.color != color).map(threatens(_, kingPosition)).reduceLeft(_ || _)
          }
        }
      }

    )

    runBenchmark(perfts, programs)
    runBenchmark(perfts, programs)
  }
}
