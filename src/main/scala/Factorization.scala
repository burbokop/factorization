import scala.annotation.tailrec

object Factorization {
  @tailrec
  def iterator(value: BigInt, x: BigInt, level: Int = 0): (BigInt, BigInt) = {
    val y = Math.sqrt((x * x - value).toDouble)
    if (y.isWhole) (x, y.toInt)
    else iterator(value, x + 1, level + 1)
  }

  def fermatFactorizationAsSeq(value: BigInt): Seq[BigInt] = {
    val y = Math.sqrt(value.toDouble)
    if (y.isWhole) {
      Seq(y.toInt, y.toInt)
    } else {
      val result = iterator(value, Math.ceil(y).toInt)
      Seq(result._1 - result._2, result._1 + result._2)
    }
  }

  def recursiveFermatFactorization(value: BigInt): Seq[BigInt] = {
    val rs = fermatFactorizationAsSeq(value)
      rs.map[Seq[BigInt]]( num =>
      if (PrimeNumbers.isPrime(num)) {
        Seq(num)
      } else {
        recursiveFermatFactorization(num)
      }
    )
      .reduce((a, b) => a ++ b)
  }
}



