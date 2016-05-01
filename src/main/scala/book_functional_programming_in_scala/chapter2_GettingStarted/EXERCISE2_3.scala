package book_functional_programming_in_scala.chapter2_GettingStarted

object EXERCISE2_3 {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a: A => b: B => f(a, b): C
  }

  //test curry
  def sum(a: Int, b: Int): Int = a + b
  val x = curry(sum)(1)(2)
  println(x)
}
