object Main {
  def main(args: Array[String]) {
    println("Hello, world!")
    val ls = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

    println("P01) This should be 9: " + last(ls))
    println("P02) This should be 8: " + penultimate(ls))
    println("P03) This should be 4: " + nth(3, ls))
    println("P04a) This should be 9: " + length(ls))
    println("P04b) This should be 9: " + recLength(ls))
    println("P05) This should be List(9, 8, 7, 6, 5, 4, 3, 2, 1): " + reverse(ls))
    println("P06b) This should be true: " + isPalindrome("amanaplanacanalpanama".toList))
    println("P06b) This should be false: " + isPalindrome("derp".toList))
    println("P07a) This should be List(1, 2, 3, 4, 5, 6, 7, 8, 9): " + flatten(List(List(1, List(2, List(3))), List(4), 5, 6, List(List(List(7), List(8), List(9))))))
    println("P07b) This should be List(1, 2, 3, 4, 5, 6, 7, 8, 9): " + flattenEZ(List(List(1, List(2, List(3))), List(4), 5, 6, List(List(List(7), List(8), List(9))))))
    println("P08) This should be List('a, 'b, 'c, 'a, 'd, 'e): " + compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println("P09) This should be List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)): \n      " + pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println("P10) This should be List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)):\n      " + encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println("P11) This should be List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)): \n      " + encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println("P12) This should be List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e): \n      " + decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
  }

  // P01
  def last[A](ls: List[A]): A = ls match {
    case h :: Nil  => h
    case _ :: tail => last(tail)
    case _         => throw new NoSuchElementException
  }

  // P02
  def penultimate[A](ls: List[A]): A = ls match {
    case h :: _ :: Nil => h
    case _ :: tail     => penultimate(tail)
    case _             => throw new NoSuchElementException
  }

  // P03
  def nth[A](index: Int, ls: List[A]): A = (index, ls) match {
    case (0, h :: _   ) => h
    case (i, h :: rest) => nth(i - 1, rest)
    case (_, Nil      ) => throw new NoSuchElementException
  }

  // P04
  def recLength[A](ls: List[Any]): Int = ls match {
    case Nil       => 0
    case _ :: rest => 1 + recLength(rest)
    case _         => throw new NoSuchElementException
  }
  def length[A](ls: List[Any]): Int = ls.foldLeft(0) { (count, _) => count + 1 }

  // P05
  def reverse[A](ls: List[Any]): List[Any] = ls match {
    case h :: Nil      => List(h)
    case h :: rest     => reverse(rest) ::: List(h)
  }

  // P06
  def isPalindrome[A](ls: List[Any]): Boolean = ls == reverse(ls)

  // P07
  def flatten[A](ls: List[Any]): List[Any] = ls match {
    case (h: List[_]) :: Nil  => flatten(h)
    case h :: Nil             => List(h)
    case (h: List[_]) :: rest => flatten(h) ::: flatten(rest)
    case h :: rest            => List(h) ::: flatten(rest)
  }
  def flattenEZ[A](ls: List[Any]): List[Any] = ls flatMap {
    case nestedList: List[_] => flatten(nestedList)
    case h                   => List(h)
  }

  // P08
  def compress[A](ls: List[Any]): List[Any] = ls.foldRight(List[Any]()) { (item, uniques) =>
    if (uniques.isEmpty || uniques.head != item) item :: uniques
    else uniques
  }

  // P09
  def pack[A](ls: List[Any]): List[List[Any]] = ls.foldRight(List[List[Any]]()) { (item, packed) =>
    if (packed.isEmpty || packed.head.head != item) List(item) :: packed
    else (item :: packed.head) :: packed.tail
  }

  // P10
  def encode[A](ls: List[Any]): List[(Int, Any)] = ls.foldRight(List[(Int, Any)]()) { (item, encoded) =>
    if (encoded.isEmpty || encoded.head._2 != item) (1, item) :: encoded
    else (encoded.head._1 + 1, item) :: encoded.tail
  }

  // P11
  def encodeModified[A](ls: List[Any]): List[Any] = encode(ls) map { (tup) =>
    if (tup._1 == 1) tup._2 else tup
  }

  // P12
  def decode[A](ls: List[(Int, A)]): List[Any] = ls.foldRight(List[Any]()) { (tup, decoded) =>
    Range(0, tup._1).foldRight(List[Any]()) { (_, sublist) => tup._2 :: sublist } ::: decoded
  }
}
