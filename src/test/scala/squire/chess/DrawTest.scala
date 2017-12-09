package squire.chess

import org.scalatest.FunSpec
import squire.base.Finished

// scalastyle:off magic.number

class DrawTest extends FunSpec {

  def knightBackAndForth(state: ChessState, n: Int, isLeftKnight: Boolean = true): ChessState = {
    val shift = if(isLeftKnight) 0 else 5
    val movePattern = Seq(
      RegularChessMove(Position(0, 1 + shift), Position(2, 2 + shift)),
      RegularChessMove(Position(7, 1 + shift), Position(5, 2 + shift)),
      RegularChessMove(Position(2, 2 + shift), Position(0, 1 + shift)),
      RegularChessMove(Position(5, 2 + shift), Position(7, 1 + shift))
    )
    val q: Int = n / 4
    val moveSeq = Seq.fill(q)(movePattern).flatten ++ movePattern.take(n - q * 4)
    moveSeq.foldLeft(state) { case (state, move) => state.apply(move) }
  }

  describe("Concerning draw by threefold repetition") {

    it("should not recognize a draw too early") {
      assert(knightBackAndForth(ChessState(), 7).evaluate != Finished(0))
    }

    it("should recognize a draw when it has just appeared") {
      assert(knightBackAndForth(ChessState(), 8).evaluate == Finished(0))
    }

    it("should not recognize a draw anymore when it is over") {
      val someMove = RegularChessMove(Position(0, 6), Position(2, 7))
      assert(knightBackAndForth(ChessState(), 8).apply(someMove).evaluate != Finished(0))
    }

    it("should recognize a draw when it happens again") {
      val  state = knightBackAndForth(knightBackAndForth(ChessState(), 8), 4, false)
      assert(state.evaluate == Finished(0))
    }
  }
}
