package squire


import squire.agent.Agent


case class Feature(val description: String, val value: Double)


abstract class GameState[+Move] {
    def apply[CovMove >: Move](move: CovMove): GameState[CovMove] = {throw new IllegalArgumentException}
    def possibleMoves: List[Move]
    def evaluate: Option[Int]
    def toString: String
    def turn: Int
    def features: Array[Feature]
}


class Game[Move](var state: GameState[Move], val player_0: Agent[Move], val player_1: Agent[Move], val maxTurns: Int) {

    def turn(): Option[Int] = {
        state = state.apply(player_0.play(state))
        println(player_0.toString + " did:")
        println(state.toString)
        println("")

        val score = state.evaluate
        if(score != None) return score

        state = state.apply(player_1.play(state))
        println(player_1.toString + "did:")
        println(state.toString)
        println("")

        val score_2 = state.evaluate
        if(score_2 != None) return score_2

        None
    }

    def play(): Unit = {
        var nTurns = 0
        var score: Option[Int] = None
        while(score == None && nTurns <= maxTurns) {
            score = turn()
            nTurns += 1
        }
        score match {
            case Some(1) => println(player_0.toString + " won")
            case Some(0) => println("It's a draw!")
            case Some(-1) => println(player_1.toString + " won")
            case _ => println("Time out...")
        }
    }
}
