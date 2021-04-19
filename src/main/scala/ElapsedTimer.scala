

class ElapsedTimer {
  val startTimeNano = System.nanoTime()
  def elapsedMillis(): Double = (System.nanoTime() - startTimeNano) * 0.001 * 0.001
  def elapsedNano(): Double = System.nanoTime() - startTimeNano
}

object ElapsedTimer {
  case class MeasureResult[T](value: T, millis: Double, nanos: Double)
  def measure[T](f: () => T): MeasureResult[T] = {
    val timer = new ElapsedTimer()
    val value = f()
    MeasureResult(value, timer.elapsedMillis(), timer.elapsedNano())
  }
}
