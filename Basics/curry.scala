object Curry {

  def sumInts(start: Int, stop: Int): Int =
    if (start > stop) 0 else start + sumInts(start + 1, stop)

  def sumFn(f: Int => Int, a: Int, b: Int) =
    f(a) + f(b)

  def main(args: Array[String]) {
    println(sumInts(1, 5))

    def square(x: Int): Int = x * x
    def cube(x: Int): Int = x * x * x
    def fact(x: Int): Int =
      if (x == 0) 1 else x * fact(x - 1)
    println(sumFn(square, 5, 5))
    println(sumFn(cube, 5, 5))
    println(sumFn(fact, 2, 2))

  }
}
