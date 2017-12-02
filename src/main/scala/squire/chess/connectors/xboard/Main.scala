package squire.chess.connectors.xboard

import akka.actor.ActorSystem
import squire.agents.minimax.NegamaxAgent
import squire.chess.heuristics.tradeValue

import scala.io.StdIn.readLine

object Main extends App {
  val system = ActorSystem("XBoardSystem")
  // default Actor constructor
  val agent = new NegamaxAgent(tradeValue, 3)
  val engineProcess = system.actorOf(EngineProcess.props(agent), name = "EngineProcess")
  Stream.continually(readLine()).foreach(engineProcess ! _)
}
