package book_functional_programming_in_scala.chapter5_StrictnessAndLaziness

import Stream._

object InfiniteStreams extends App {
  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def toStream(x: Int, y: Int): Stream[Int] = cons(x, toStream(y, x + y))
    toStream(1, 1)
  }



  println(ones.takeN(5).toList)
  println(ones.existsLazy(_ % 2 != 0))
  println(ones.mapLazy(_ + 1).existsLazy(_ % 2 == 0))
  println(ones.takeWhileLazy(_ == 1))
  println(ones.forAllLazy(_ != 1))
  println("Fibonaci:" + fibs.takeN(5))
}
