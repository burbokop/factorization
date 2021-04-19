
object PrimeNumbers {
  def isPrime(i: Int) =
    i > 1 && (2 until i).takeWhile(i % _ != 0).find(i == _ + 1).isDefined

  def isPrime(i: BigInt) =
    i > 1 && (BigInt(2) until i).takeWhile(i % _ != 0).find(i == _ + 1).isDefined

  def generateSeq(max: Int) =
    (2 until max).filter(isPrime(_))

  def generateBigIntSeq(max: BigInt) =
    (BigInt(2) until max).filter(isPrime(_))
}
