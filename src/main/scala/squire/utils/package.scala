package squire

package object utils {

  def time[R](block: => R): (R, Double) = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    (result, (t1 - t0) / 1e6)
  }

}
