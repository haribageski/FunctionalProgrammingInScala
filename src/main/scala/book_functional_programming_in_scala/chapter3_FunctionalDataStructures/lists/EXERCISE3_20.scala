package book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists

import book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists.EXERCISE3_13._

object EXERCISE3_20 {
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRightInTermsOfFoldLeft(as, List[B]())((acc, elem) => f(elem) ::: acc)
    //we achieve same time complexity as with foldLeft
  }
}

