package book_functional_programming_in_scala

object Chapter2_GettingStarted{

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def compareTwoElementsAtIndex(index: Int): Boolean = {
      if(index > as.length) true
      else  if(!ordered(as(index - 1) , as(index)))   false
      else  compareTwoElementsAtIndex(index + 1)
    }
    compareTwoElementsAtIndex(1)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a: A => b: B => f(a, b): C
  }

  def uncurry[A,B,C](f: A => (B => C)): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

  //test curry
  def sum(a: Int, b: Int): Int = a + b
  val x = curry(sum)(1)(2)
  println(x)

  //test uncurry
  def sumWithCurrying(a: Int)(b: Int): Int = a + b
  val y = uncurry(sumWithCurrying)(1, 2)
  println(y)

}
