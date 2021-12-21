package util

object ElapsedTimeUtil {
  def measureElapsedTimeInMillis[T](simulation: => T): (T, Long) = {
    val start  = System.currentTimeMillis()
    val result = simulation
    val end    = System.currentTimeMillis()

    (result, end - start)
  }
  def measureElapsedTimeInNanos[T](simulation: => T): (T, Long)  = {
    val start  = System.nanoTime()
    val result = simulation
    val end    = System.nanoTime()

    (result, end - start)
  }
}
