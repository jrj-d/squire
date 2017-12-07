package squire.chess

case class MoveCount(
                      leaves: Long,
                      enPassant: Long,
                      castling: Long,
                      promotion: Long,
                      check: Long,
                      checkMate: Long) {

  def +(c: MoveCount): MoveCount = MoveCount(
    this.leaves + c.leaves,
    this.enPassant + c.enPassant,
    this.castling + c.castling,
    this.promotion + c.promotion,
    this.check + c.check,
    this.checkMate + c.checkMate
  )
}

object MoveCount {

  def fromSeq(l: Seq[Long]): MoveCount = MoveCount(l(0), l(1), l(2), l(3), l(4), l(5))

  def empty: MoveCount = MoveCount(0, 0, 0, 0, 0, 0)
  def enPassant: MoveCount = MoveCount(1, 1, 0, 0, 0, 0)
  def castling: MoveCount = MoveCount(1, 0, 1, 0, 0, 0)
  def promotion: MoveCount = MoveCount(1, 0, 0, 1, 0, 0)
  def regular: MoveCount = MoveCount(1, 0, 0, 0, 0, 0)
  def check: MoveCount = MoveCount(0, 0, 0, 0, 1, 0)
  def checkMate: MoveCount = MoveCount(0, 0, 0, 0, 1, 1)
}
