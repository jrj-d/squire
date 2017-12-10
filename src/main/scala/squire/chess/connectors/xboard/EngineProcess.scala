package squire.chess.connectors.xboard

import com.typesafe.scalalogging.LazyLogging
import squire.agents.TimeLimitedAgent
import squire.agents.minimax._
import squire.base.Agent
import squire.chess.ChessState
import squire.chess.heuristics._

import scala.io.StdIn.readLine

// scalastyle:off cyclomatic.complexity
// scalastyle:off method.length
// scalastyle:off magic.number

object Option extends Enumeration {
  type Option = Value

  val ModelType     = Value("Model type")
  val HeuristicType = Value("Heuristic type")
}

object ModelType extends Enumeration {
  type ModelType = Value

  val Negamax       = Value("negamax")
  val AlphaBeta     = Value("alpha-beta pruning")
  val MovesOdering  = Value("alpha-beta pruning + moves ordering")
  val Quiescence    = Value("alpha-beta pruning + moves ordering + quiescence search")
}

object HeuristicType extends Enumeration {
  type HeuristicType = Value

  val TradeValue  = Value("trade value")
  val Michniewski = Value("michniewski")
}

class EngineProcess extends LazyLogging {

  var forced = false
  var state = ChessState()
  var timePerMove: Double = 7500
  var modelType: ModelType.Value = ModelType.MovesOdering
  var heuristicType: HeuristicType.Value = HeuristicType.Michniewski
  var maxDepth = 100
  var agent: Agent[ChessState] = _

  def setAgent(): Unit = {
    val heuristic: ChessState => Double = heuristicType match {
      case HeuristicType.TradeValue => tradeValue
      case HeuristicType.Michniewski => michniewski
    }
    val baseAgent: Int => Agent[ChessState] =
      modelType match {
        case ModelType.Quiescence =>
          (d: Int) => new QuiescentAlphaBetaNegamaxAgent[ChessState](heuristic, isNotCaptureNorCheckNorPromotion, d) with MovesOrdering[ChessState]
        case ModelType.MovesOdering =>
          (d: Int) => new AlphaBetaNegamaxAgent[ChessState](heuristic, d) with MovesOrdering[ChessState]
        case ModelType.AlphaBeta =>
          (d: Int) => new AlphaBetaNegamaxAgent[ChessState](heuristic, d)
        case ModelType.Negamax =>
          (d: Int) => new NegamaxAgent[ChessState](heuristic, d)
      }
    agent = new TimeLimitedAgent(
      baseAgent,
      1 to maxDepth,
      timePerMove.toLong
    )
    logger.info(s"Setting agent with model '$modelType', heuristic '$heuristicType', max depth $maxDepth, time per move $timePerMove ms")
  }

  def features: Seq[String] = {
    val modelChoices = ModelType.values.toSeq.mkString(" /// ")
    val modelTypeString = s"""feature option="${Option.ModelType} -combo $modelChoices""""
    val heuristicChoices = HeuristicType.values.toSeq.mkString(" /// ")
    val heuristicTypeString = s"""feature option="${Option.HeuristicType} -combo $heuristicChoices""""
    Seq(
      "feature done=0",
      "feature myname=\"Squire\"",
      "feature usermove=1",
      "feature setboard=1",
      "feature ping=1",
      "feature sigint=0",
      "feature san=0",
      "feature variants=\"normal\"",
      modelTypeString,
      heuristicTypeString,
      "feature done=1"
    )
  }

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
        features.foreach(printAndLog)
        setAgent()

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
          case None => printAndLogError("Error (ambiguous move): " + words(1))
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
        setAgent()

      case s: String if s.startsWith("option") =>
        val theRest = s.stripPrefix("option ")
        val words = theRest.split("=")
        Option.values.find(o => o.toString == words(0)) match {
          case Some(Option.ModelType) => ModelType.values.find(m => m.toString == words(1)) match {
            case Some(model) => modelType = model
            case None => logger.error(s"Did not understand model type '${words(1)}'")
          }
          case Some(Option.HeuristicType) => HeuristicType.values.find(m => m.toString == words(1)) match {
            case Some(heuristic) => heuristicType = heuristic
            case None => logger.error(s"Did not understand heuristic type '${words(1)}'")
          }
          case None => logger.error(s"Did not understand option '${words(0)}'")
        }
        setAgent()

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
                printAndLogError("Error (bad time description): " + s)
            }
          }
        }
        setAgent()

      case s: String if s.startsWith("sd") =>
        val words = s.split(" ")
        maxDepth = words(1).toInt
        setAgent()

      case s => printAndLogWarning("Error (unkown command): " + s)
    }
  }

  def logMessage(message: String): Unit = {
    logger.info(s"XBoard sends> $message")
  }

  def printAndLog(s: String): Unit = {
    logger.info(s"Engine answers> $s")
    println(s) // scalastyle:ignore regex
  }

  def printAndLogWarning(s: String): Unit = {
    logger.warn(s"Engine answers> $s")
    println(s) // scalastyle:ignore regex
  }

  def printAndLogError(s: String): Unit = {
    logger.error(s"Engine answers> $s")
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
