package squire.base

trait Agent[S <: State[S]] {
  def play(state: S): S#Move
}
