package squire.agents

import squire.base.{Agent, State}

class RandomAgent[Move](seed: Long) extends Agent[Move] {

  val random = new scala.util.Random(seed)

  def play(state: State[Move]): Move = {
    val moves = state.possibleMoves
    moves(random.nextInt(moves.length))
  }

}

