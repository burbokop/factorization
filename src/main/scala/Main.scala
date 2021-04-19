
import org.nspl.{line, _}
import awtrenderer._

import scala.language.postfixOps

object Main extends App {
  {
    val num = 272923
    val result = Factorization.recursiveFermatFactorization(num)
    println(s"num: $num -> $result")
  }

  def testIterationsCount(numOfMultipliers: Int, maxPrimeNumber: Int) = {
    val primeNumbers = PrimeNumbers.generateBigIntSeq(maxPrimeNumber)
      val multipliers = (0 until numOfMultipliers).map( _ => primeNumbers((Math.random() * primeNumbers.length).toInt))
      val inputNumber = multipliers.reduce((a, b) => a * b)
      val result = ElapsedTimer.measure { () =>
        Factorization.recursiveFermatFactorization(inputNumber)
      }
      result.millis
  }

  val distanceSeq = (2 until 35)
  val iterationSeq = distanceSeq.map(testIterationsCount(_, 5))

  println(distanceSeq)
  println(iterationSeq)

  val plot = xyplot(
    distanceSeq.map(_.toDouble) -> iterationSeq -> line(stroke = StrokeConf(0.1 fts), color = Color(255, 0, 255)),
  )(
    ylab = "Count of multipliers",
    xlab = "miliseconds",
    main = "dependence between prime multipliers count and elapsed time",
  )

  show(plot)
}
