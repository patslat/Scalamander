object Session {
  def abs(x: Double) = if (x >= 0) x else -x
  def square(x: Double) = x * x

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double): Boolean =
      (abs(guess * guess - x) / x) < 0.001

    def improve(guess: Double): Double =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  def main(args: Array[String]) {
    val x = sqrt(5)
    println(x)
  }
}
// vim: set ts=2 sw=2 et:
