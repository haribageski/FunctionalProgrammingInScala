package book_functional_programming_in_scala.chapter5_StrictnessAndLaziness

import book_functional_programming_in_scala.chapter5_StrictnessAndLaziness.Stream.cons

object NonSharedStateWithUnfold {
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((value, state)) => cons(value, unfold(state)(f))
      case None => Stream.empty
    }
  }


  val fibs = unfold(Stream(1, 1))((stream: Stream[Int]) => stream match {
    case Cons(h, t) =>
      val h2 = t().headOptionLazy()
      h2.map(z => (h(), Stream(z, z + h())))
    case Empty => None
  })


  def from(n: Int): Stream[Int] = unfold(Stream(n))(_.headOptionLazy().map(z => (z, Stream(z+1))))



  println("Fibonaci2:" + fibs.takeN(5))
  println(from(-10).takeN(15))

  val naturalNums: Stream[Int] = from(0)
  println(unfold(naturalNums)((state: Stream[Int]) => state match {
    case Cons(h, t) => {
      if (h() < 10)
        Some(h(), t())
      else
        None
    }
    case Empty => None
  }).takeN(10000000))
}
