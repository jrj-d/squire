package squire.base

trait Evaluation
case object Playing extends Evaluation
case class Finished(result: Double) extends Evaluation

trait State[S <: State[S]] { self: S =>

  type Move

  def apply(move: S#Move): S

  def possibleMoves: Seq[S#Move]

  def evaluate: Evaluation

  def currentPlayer: Int

}
