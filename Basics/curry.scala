object Curry {

  def sumInts(start: Int, stop: Int): Int = {
    if (start > stop) 0 else start + sumInts(start + 1, stop)
  }

  def sumFn(f: Int => Int, a: Int, b: Int) = {

  }

  def main(args: Array[String]) {
    println(sumInts(1, 5))
  }
}
