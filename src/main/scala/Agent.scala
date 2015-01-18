package squire.agent

import squire._
import squire.chess._
import squire.chess.Color._
import regression._

import scala.math.{max, tanh}
import java.text.DecimalFormat


case class TimeOutException(message: String) extends Exception(message)


abstract class Agent[Move] {
    def play(state: GameState[Move]): Move
}


class RandomAgent[Move] extends Agent[Move] {
	def play(state: GameState[Move]): Move = {
		val moves = state.possibleMoves
		moves(scala.util.Random.nextInt(moves.length))
	}
}


class SimpleEstimateAgent[Move](val estimator: Regressor) extends Agent[Move] {
	def play(state: GameState[Move]): Move = {

		val moves = state.possibleMoves

		var bestMove: Move = state.possibleMoves(0)
		var bestScore = Double.MinValue

		for(move <- moves) {
			val newState = state.apply(move)
			newState.evaluate match {
				case Some(c) if c == 1 - 2 * (state.turn % 2) => return move
				case Some(_) => {}
				case None => {
					val score = (1 - 2 * (state.turn % 2)) * estimator(newState.features.map(_.value))
					if(score > bestScore) {
						bestMove = move
						bestScore = score
					}
				}
			}
		}

		return bestMove
	}
}


class ChessHumanPlayer extends Agent[ChessMove] {
	override def play(state: GameState[ChessMove]): ChessMove = {

		val chessState = state.asInstanceOf[ChessState] // horrible but I can't handle Scala's inheritance of parameterized methods
		val pattern = """([a-h])([1-8])""".r

		def decodeMove(code: String): Option[Position] = {
			code match {
				case pattern(col_str, row_str) => Some(Position((row_str.charAt(0) - '1'):Int, (col_str.charAt(0) - 'a'):Int))
				case _ => None
			}
		}

		def wrongFormat(reason: String) = println("I did not understand your choice (" + reason + ")")
		def forbidden(reason: String) = println("This move is not valid (" + reason + ")")

		val moves = chessState.possibleMoves
		val color = if(chessState.turn % 2 == 0) White else Black
		while(true) {
			val input = readLine("Your move? ")
			input match {
				case "left castling" => {
					val row = if(color == White) 0 else 7
					val move = Castling(Position(row, 4), Position(row, 0))
					if(moves.contains(move)) return move
					else forbidden("you cannot perform a left castling")
				}
				case "right castling" => {
					val row = if(color == White) 0 else 7
					val move = Castling(Position(row, 4), Position(row, 7))
					if(moves.contains(move)) return move
					else forbidden("you cannot perform a right castling")
				}
				case _ => {
					val words = input.split(" ")
					if(words.length == 2) {
						val srcOption = decodeMove(words(0))
						val destOption = decodeMove(words(1))
						if(destOption != None && srcOption != None) {
							val Some(src) = srcOption
							val Some(dest) = destOption
							val piece = chessState.board(src.row)(src.column)
							if(piece != null) {
								val move = RegularChessMove(src, dest)
								if(moves.contains(move)) return move
								else forbidden("not found among valid moves")
							} else forbidden("did not find source piece")
						} else wrongFormat("bad coding for positions")
					} else if (words.length == 3) {
						val srcOption = decodeMove(words(0))
						val destOption = decodeMove(words(1))
						val promoted = words(2)
						if(destOption != None && srcOption != None) {
							val Some(src) = srcOption
							val Some(dest) = destOption
							val piece = chessState.board(src.row)(src.column)
							piece match {
								case Pawn(c, n) => {
									val move = promoted match {
										case "r" => Promotion(src, 'r', dest)
										case "n" => Promotion(src, 'n', dest)
										case "b" => Promotion(src, 'b', dest)
										case _ => Promotion(src, 'q', dest)
									}
									if(moves.contains(move)) return move
									else forbidden("not found among valid moves")
								}
								case _ => wrongFormat("did not find source piece or source piece not a pawn")
							}
						} else wrongFormat("bad coding for positions")
					} else wrongFormat("number of words not right")
				}
			}
		}
		null
	}
}


class MinimaxAgent[Move](val estimator: Regressor, var maxTime: Double) extends Agent[Move] {

	var nodeCounter = 0
	var global_init_time: Long = 0

	def setTimePerMove(time: Double) = {maxTime = time}

	def play(state: GameState[Move]): Move = {
		global_init_time = System.currentTimeMillis
		var depth = 1
		var bestMove: Option[Move] = None
		try {
	   		for(depth <- 1 to 100)
				bestMove = Some(findBestMove(state, depth))
		} catch {
		    case e: TimeOutException => ()
		    case e: Exception => throw e
   		}
   		bestMove.get
	}

	def updateCounters() = {
		nodeCounter += 1
		if(System.currentTimeMillis - global_init_time >= maxTime * 60000)
			throw TimeOutException("")
	}

	def findBestMove(state: GameState[Move], depth: Int): Move = {

		val formatter = new DecimalFormat("#.##")

		nodeCounter = 1
		val init_time = System.currentTimeMillis

		def compPair(t1: Tuple2[Move, Double], t2: Tuple2[Move, Double]) =
			if(t1._2 > t2._2) t1
			else t2

		val bestMove = getPossibleMoves(state)
			.map {m => (m, state.apply(m))}
			.map {case (m, s) => (m, -negamax(s, depth - 1, -1 + 2 * (state.turn % 2)))}
			.reduceLeft(compPair)
			._1

		val duration = (System.currentTimeMillis - init_time) / 60000.0

		println("# " + depth + "-ply depth, " + nodeCounter + " states evaluated in " + formatter.format(duration) + " minutes -> " + bestMove.toString)

		return bestMove
	}

	def getPossibleMoves(state: GameState[Move]) = state.possibleMoves

	def negamax(state: GameState[Move], depth: Int, color: Int): Double = {
		updateCounters()
		state.evaluate match {
			case Some(v) => color * v
			case None =>
				if(depth == 0) tanh(color * estimator(state.features.map(_.value)) / 100.0)
				else getPossibleMoves(state)
					.map (state.apply(_))
					.map (-negamax(_, depth - 1, -color))
					.reduceLeft (max(_, _))
		}
	}

}

trait AlphaBetaPruning[Move] { self: MinimaxAgent[Move] =>

	override def findBestMove(state: GameState[Move], depth: Int): Move = {

		val formatter = new DecimalFormat("#.##")

		nodeCounter = 1
		val init_time = System.currentTimeMillis

		var bestValue = Double.MinValue
		var currentAlpha = Double.MinValue
		var bestMove: Option[Move] = None
		for(move <- getPossibleMoves(state)) {
			val currentValue = -negamax(state.apply(move), depth - 1, -1 + 2 * (state.turn % 2), Double.MinValue, -currentAlpha)
			println("# " + currentValue + " -> " + move.toString)
			if(currentValue >= bestValue) {
				bestValue = currentValue
				bestMove = Some(move)
			}
			currentAlpha = max(currentAlpha, currentValue)
		}

		val duration = (System.currentTimeMillis - init_time) / 60000.0

		println("# " + depth + "-ply depth, " + nodeCounter + " states evaluated in " + formatter.format(duration) + " minutes -> " + bestMove.get.toString + " (" + bestValue + ")")
		return bestMove.get
	}

	def negamax(state: GameState[Move], depth: Int, color: Int, alpha: Double, beta: Double): Double = {
		updateCounters()
		state.evaluate match {
			case Some(v) => color * v
			case None =>
				if(depth == 0) tanh(color * estimator(state.features.map(_.value)) / 100.0)
				else {
					var bestValue = Double.MinValue
					var currentAlpha = alpha
					for(move <- getPossibleMoves(state)) {
						if(currentAlpha < beta) {
							val currentValue = -negamax(state.apply(move), depth - 1, -color, -beta, -currentAlpha)
							bestValue = max(bestValue, currentValue)
							currentAlpha = max(currentAlpha, currentValue)
						}
					}
					return bestValue
				}
		}
	}
}


trait MovesOrdering[Move] { self: MinimaxAgent[Move] =>

	override def getPossibleMoves(state: GameState[Move]) = {
		def compPair(t1: Tuple2[Move, Double], t2: Tuple2[Move, Double]) = t1._2 > t2._2
		val color = 1 - 2 * (state.turn % 2)
		val moves = state.possibleMoves
		(moves zip {
			moves.map(m => color * estimator(state.apply(m).features.map(_.value)))
		})
			.sortWith(compPair)
			.map{case (m,v) => m}
	}
}


object Default {
	def estimator: Regressor = {
		var coefficients = Array(-10.0, 1, 5, 3, 3, 9, // check + # pieces
								 -1, 1, -1, 1, -1, 1, -2, 2, // first pieces (threat + def)
								 -1, 1, -1, 1, -1, 1, -2, 2) // second pieces (threat + def)
		coefficients = coefficients ++ coefficients.map(-1 * _)
		return new Ridge(coefficients)
	}
}
