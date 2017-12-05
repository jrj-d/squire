package squire.chess.connectors.xboard

import squire.agents.TimeLimitedAgent
import squire.agents.minimax.AlphaBetaNegamaxAgent
import squire.base.Agent
import squire.chess.ChessState
import squire.chess.heuristics.tradeValue

// scalastyle:off magic.number

object Main extends App {
  // default Actor constructor
  val agent: Agent[ChessState] = new TimeLimitedAgent(
    (depth: Int) => new AlphaBetaNegamaxAgent[ChessState](tradeValue, depth),
    1 to 10,
    7500
  )
  //val agent = new NegamaxAgent(tradeValue, 1)
  val engineProcess = new EngineProcess(agent)
  engineProcess.run()
}
