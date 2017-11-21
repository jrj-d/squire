

package object old {

	var continue = true
	var forced = false
	var state = ChessState()
	val engine = new MinimaxAgent[ChessMove](Default.estimator, 1.0)
		with AlphaBetaPruning[ChessMove]
		with MovesOrdering[ChessMove]
	var time = 1
	var otim = 1

	def play(): String = {
		val move = engine.play(state)
		val moveString = state.encodeANMove(move)
		state = state.apply(move)
		moveString
	}

	def decodeTime(s: String) = s.split(":").length match {
		case 1 => Some(s.toDouble)
		case 2 => {
			val words = s.split(":")
			Some(words(0).toDouble + words(1).toDouble / 60.0)
		}
		case _ => None
	}

	def main(args: Array[String]) {

    	while(continue) {
    		readLine() match {

    			case "quit" => continue = false

    			case "new" => {
    				forced = false
    				state = ChessState()
    			}

    			case "force" => forced = true

    			case "go" => {
    				forced = false
    				println("move " + play())
    			}

    			case "protover 2" => {
    				println("feature done=0")
					println("feature myname=\"Squire\"")
					println("feature usermove=1")
					println("feature setboard=1")
					println("feature ping=1")
					println("feature sigint=0")
					println("feature san=0")
					println("feature variants=\"normal\"")
					println("feature done=1")
    			}

    			case s if s.startsWith("setboard") => {
    				val words = s.split(" ", 2)
    				state = ChessState.parseFen(words(1))
    			}

    			case s if s.startsWith("ping") => {
    				val words = s.split(" ")
    				println("pong " + words(1))
    			}

    			case s if s.startsWith("usermove") => {
    				val words = s.split(" ")
    				val move = state.decodeANMove(words(1))
    				move match {
    					case None => println("Error (ambiguous move): " + words(1))
    					case Some(move) => {
    						if(!state.possibleMoves.contains(move)) println("Illegal move: " + words(1))
    						else {
    							state = state.apply(move)
    							if(!forced) println("move " + play())
    						}
    					}
    				}
    			}

    			case s if s.startsWith("time") => {
    				val words = s.split(" ")
    				time = words(1).toInt
    			}

    			case s if s.startsWith("otim") => {
    				val words = s.split(" ")
    				otim = words(1).toInt
    			}

    			case s if s.startsWith("st") => {
    				val words = s.split(" ")
    				engine.setTimePerMove(words(1).toInt / 60.0)
    			}

    			case s if s.startsWith("level") => {
    				val words = s.split(" ")
    				words(1).toInt match {
    					case 0 => engine.setTimePerMove((words(2).toInt * 60 + 39 * words(3).toInt) / 40.0 / 60.0)
    					case n => {
    						val totalTime = decodeTime(words(2))
    						if(totalTime == None) println("Error (bad time description): " + s)
    						else engine.setTimePerMove(totalTime.get / n)
    					}
    				}
    			}

    			case s => println("Error (unkown command): " + s)
    		}
    	}
    }

}
