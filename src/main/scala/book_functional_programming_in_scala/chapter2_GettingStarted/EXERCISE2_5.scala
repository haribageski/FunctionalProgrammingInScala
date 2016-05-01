package book_functional_programming_in_scala.chapter2_GettingStarted

object EXERCISE2_5 {
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }
}
