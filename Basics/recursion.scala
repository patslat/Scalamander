object Recursion {
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def fact(n: Int): Int =
    if (n == 0) 1 else n * fact(n - 1)

  def tailFact(n: Int) = {

    def factHelper(n: Int, counter: Int): Int =
      if (counter == 0) n else factHelper(n * counter, counter - 1)

    factHelper(n, n - 1)

  }

  def main(args: Array[String]) {
    println(gcd(14, 21))

    println(tailFact(10))
    println(fact(10))
  }
}

// vim: set ts=2 sw=2 et:
