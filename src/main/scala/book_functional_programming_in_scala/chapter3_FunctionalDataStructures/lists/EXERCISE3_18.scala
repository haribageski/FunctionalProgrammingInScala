package book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists

import book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists.EXERCISE3_13._

object EXERCISE3_18 extends App{
  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRightInTermsOfFoldLeft[A,List[B]](as, List[B]())((acc, elem) => f(elem) :: acc)
    //if we use foldLeft we would reach O(N^2) complexity, although it seems more intuitive
  }
  println("ex3_18_map:" + map(List(1,2,3))(_.toString))
}

