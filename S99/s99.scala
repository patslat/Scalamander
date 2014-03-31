object Main {
  def main(args: Array[String]) {
    println("Hello, world!")
    val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val lastItem = last(list)
    println(lastItem)
  }
  def last[A](list: List[A]): A = list match {
    case h :: Nil  => h
    case _ :: tail => last(tail)
    case _         => throw new NoSuchElementException
  }
}
