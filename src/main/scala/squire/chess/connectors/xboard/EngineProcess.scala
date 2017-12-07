package squire.chess.connectors.xboard

import com.typesafe.scalalogging.LazyLogging
import squire.base.Agent
import squire.chess.ChessState

import scala.io.StdIn.readLine

// scalastyle:off cyclomatic.complexity
// scalastyle:off method.length

class EngineProcess(val agent: Agent[ChessState]) extends LazyLogging {

  var forced = false
  var state = ChessState()
  var timePerMove = 0.0

  def play(): Unit = {
    if(state.possibleMoves.nonEmpty) {
      val move = agent.play(state)
      val moveString = state.encodeAlgebraicNotationMove(move)
      logger.info(s"Agent plays $move")
      state = state.apply(move)
      logBoard()
      printAndLog(s"move $moveString")
    } else {
      logger.info("Agent cannot play because match is lost")
    }
  }

  def receive(message: String): Unit = {
    logMessage(message)

    message match {

      case "quit"  =>
        logger.info("Engine will shutdown...")
        sys.exit()

      case "protover 2" =>
        printAndLog("feature done=0")
        printAndLog("feature myname=\"Squire\"")
        printAndLog("feature usermove=1")
        printAndLog("feature setboard=1")
        printAndLog("feature ping=1")
        printAndLog("feature sigint=0")
        printAndLog("feature san=0")
        printAndLog("feature variants=\"normal\"")
        printAndLog("feature done=1")

      case s: String if s.startsWith("setboard") =>
        val words = s.split(" ", 2)
        state = ChessState.parseFen(words(1))
        logger.info(s"Board state is \n$state")

      case s: String if s.startsWith("ping") =>
        val words = s.split(" ")
        printAndLog("pong " + words(1))

      case "go" =>
        forced = false
        play()

      case "new" =>
        forced = false
        state = ChessState()
        logBoard()

      case "force" => forced = true

      case s: String if s.startsWith("usermove") =>
        val words = s.split(" ")
        val moveOption = state.decodeAlgebraicNotationMove(words(1))
        moveOption match {
          case None => printAndLog("Error (ambiguous move): " + words(1))
          case Some(move) => {
            if(!state.possibleMoves.contains(move)) {
              printAndLog("Illegal move: " + words(1))
            } else {
              state = state.apply(move)
              logger.info(s"Opponent plays $move")
              logBoard()
              if(!forced) play()
            }
          }
        }

      case s: String if s.startsWith("st") =>
        val words = s.split(" ")
        timePerMove = words(1).toInt * 1000.0

      case s: String if s.startsWith("level") =>
        val words = s.split(" ")
        words(1).toInt match {
          case 0 =>
            timePerMove = (words(2).toInt * 60 + 39 * words(3).toInt) / 40.0 * 1000.0
          case n => {
            val totalTimeOption = decodeTime(words(2))
            totalTimeOption match {
              case Some(totalTime) =>
                timePerMove = totalTime / n
              case None =>
                printAndLog("Error (bad time description): " + s)
            }
          }
        }

      case s => printAndLog("Error (unkown command): " + s)
    }
  }

  def logMessage(message: String): Unit = {
    logger.info(s"XBoard sends> $message")
  }

  def printAndLog(s: String): Unit = {
    logger.info(s"Engine answers> $s")
    println(s) // scalastyle:ignore regex
  }

  def logBoard(): Unit = {
    logger.info(s"Board state is \n$state")
  }

  def decodeTime(s: String): Option[Double] = s.split(":").length match {
    case 1 => Some(s.toDouble * 60000.0)
    case 2 => {
      val words = s.split(":")
      Some(words(0).toDouble * 60000.0 + words(1).toDouble * 1000.0)
    }
    case _ => None
  }

  def run(): Unit = {
    Stream.continually(readLine()).foreach(receive)
  }

}
