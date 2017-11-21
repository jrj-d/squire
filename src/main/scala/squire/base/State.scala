package squire.base

trait Evaluation
case object Playing extends Evaluation
case class Finished(result: Double) extends Evaluation

trait State[S <: State[S]] { self: S =>

  type Move

  def apply(move: Move): S

  def possibleMoves: Seq[Move]

  def evaluate: Evaluation

  def currentPlayer: Int

}

object State {
  implicit def apply[S: State]: State[S] = implicitly[State[S]]
}
