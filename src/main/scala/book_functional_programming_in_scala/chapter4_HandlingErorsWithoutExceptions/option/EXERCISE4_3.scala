package book_functional_programming_in_scala.chapter4_HandlingErorsWithoutExceptions.option

import book_functional_programming_in_scala.chapter4_HandlingErorsWithoutExceptions.option.EXERCISE4_1._

object EXERCISE4_3 {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aVal => b.map(f(aVal, _)))
}
