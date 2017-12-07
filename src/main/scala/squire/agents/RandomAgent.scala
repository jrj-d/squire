package squire.agents

import squire.base.{Agent, State}

class RandomAgent[S <: State[S]](seed: Long) extends Agent[S] {

  val random = new scala.util.Random(seed)

  def play(state: S): S#Move = {
    val moves = state.possibleMoves
    moves(random.nextInt(moves.length))
  }

}

