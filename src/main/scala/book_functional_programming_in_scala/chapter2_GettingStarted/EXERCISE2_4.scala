package book_functional_programming_in_scala.chapter2_GettingStarted

object EXERCISE2_4 {
  def uncurry[A,B,C](f: A => (B => C)): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  //test uncurry
  def sumWithCurrying(a: Int)(b: Int): Int = a + b
  val y = uncurry(sumWithCurrying)(1, 2)
  println(y)
}
