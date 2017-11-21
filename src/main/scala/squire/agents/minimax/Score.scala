package squire.agents.minimax

sealed trait Score extends Ordered[Score] {

  override def compare(that: Score): Int = (this, that) match {
    case (Result(0), Heuristic(thatValue)) => -Score.sign(thatValue)
    case (Heuristic(thisValue), Result(0)) => Score.sign(thisValue)
    case (Result(thisValue), Heuristic(_)) => Score.sign(thisValue)
    case (Heuristic(_), Result(thatValue)) => -Score.sign(thatValue)
    case (Result(thisValue), Result(thatValue)) => Score.sign(thisValue - thatValue)
    case (Heuristic(thisValue), Heuristic(thatValue)) => Score.sign(thisValue - thatValue)
  }

  def invert: Score = this match {
    case Result(v) => Result(-v)
    case Heuristic(v) => Heuristic(-v)
  }

}

case class Heuristic(value: Double) extends Score

case class Result(value: Double) extends Score

object Score {

  def sign(d: Double): Int = d match {
    case 0 => 0
    case v if v < 0 => -1
    case _ => 1
  }
  
}
