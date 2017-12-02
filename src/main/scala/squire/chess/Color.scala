package squire.chess

sealed abstract class Color(val id: Int) {
  def opponent: Color = this match {
    case Black => White
    case White => Black
  }
}

case object White extends Color(0)
case object Black extends Color(1)

object Color {
  def fromPlayer(i: Int): Color = i match {
    case 0 => White
    case 1 => Black
    case _ => throw new IllegalArgumentException(s"Unknown chess player numbered $i (should be 0 or 1)")
  }
}
