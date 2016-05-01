package book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists

import book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists.EXERCISE3_10._

object EXERCISE3_12 extends App{
  def reverseList[A](as: List[A]): List[A] = {
    foldLeft(as, List.empty[A])((acc, elem) => elem :: acc)
  }
  println("ex3_12ReverseList:" + reverseList(List(1, 2, 3)))
}

