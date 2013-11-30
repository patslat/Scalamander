object HigherOrderFunctions {

  def sumInts(start: Int, stop: Int): Int =
    if (start > stop) 0 else start + sumInts(start + 1, stop)

  def sumFn(f: Int => Int, a: Int, b: Int) =
    f(a) + f(b)

  def prodFn(f: Int => Int, start: Int, stop: Int): Int =
    if (stop - start < 0) 1 else f(start) * prodFn(f, start + 1, stop)

  def fnOfFn(acc: (Int, Int) => Int, fn: Int => Int, id: Int, start: Int, stop: Int): Int =
    if (stop - start < 0)
      id
    else
      acc(
        fn(start),fnOfFn(acc, fn, id, start + 1, stop)
      )

  def main(args: Array[String]) {
    println(sumInts(1, 5))

    def id(x: Int) = x
    def square(x: Int) = x * x
    def cube(x: Int) = x * x * x
    def fact(x: Int): Int =
      if (x == 0) 1 else x * fact(x - 1)

    println(sumFn(id, 5, 5))
    println(sumFn(x => x, 5, 5))

    println(sumFn(square, 5, 5))
    println(sumFn(x => x*x, 5, 5))

    println(sumFn(cube, 5, 5))
    println(sumFn(x => x*x*x, 5, 5))

    println(sumFn(fact, 2, 2))

    println(prodFn(id, 1, 2))
    println(prodFn(x => x, 1, 2))
    println(prodFn(square, 1, 5))
    println(prodFn(x => x*x, 1, 5))
    println(prodFn(cube, 1, 5))

    println(prodFn(x => x, 1, 3))

    println(
      fnOfFn(
        (x: Int, y: Int) => (x + y),
        (x:Int) => x,
        0,
        1,
        3
      )
    )

    println(
      fnOfFn(
        (x: Int, y: Int) => (x * y),
        (x: Int) => x * x * x,
        1,
        1,
        5
      )
    )
  }
}
