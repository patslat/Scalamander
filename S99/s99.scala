object Main {
  def main(args: Array[String]) {
    println("Hello, world!")
    val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

    println("P01) This should be 9: " + last(list))
    println("P02) This should be 8: " + penultimate(list))
    println("p03) This should be 4: " + nth(3, list))
    println("p04a) This should be 9: " + length(list))
    println("p04b) This should be 9: " + recLength(list))
    println("p05) This should be List(9, 8, 7, 6, 5, 4, 3, 2, 1): " + reverse(list))
  }

  // P01
  def last[A](list: List[A]): A = list match {
    case h :: Nil  => h
    case _ :: tail => last(tail)
    case _         => throw new NoSuchElementException
  }

  // P02
  def penultimate[A](list: List[A]): A = list match {
    case h :: _ :: Nil => h
    case _ :: tail     => penultimate(tail)
    case _             => throw new NoSuchElementException
  }

  // P03
  def nth[A](index: Int, list: List[A]): A = (index, list) match {
    case (0, h :: _   ) => h
    case (i, h :: rest) => nth(i - 1, rest)
    case (_, Nil      ) => throw new NoSuchElementException
  }

  // P04
  def recLength[A](list: List[A]): Int = list match {
    case Nil       => 0
    case _ :: rest => 1 + recLength(rest)
    case _         => throw new NoSuchElementException
  }

  def length[A](list: List[A]): Int = {
    list.foldLeft(0) { (count, _) => count + 1 }
  }

  def reverse[A](list: List[A]): List[A] = list match {
    case h :: Nil      => List(h)
    case h :: rest     => reverse(rest) ::: List(h)
  }
}
