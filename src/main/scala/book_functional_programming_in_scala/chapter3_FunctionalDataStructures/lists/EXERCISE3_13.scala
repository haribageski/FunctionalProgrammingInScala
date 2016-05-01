package book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists

import book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists.EXERCISE3_10._
import book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists.EXERCISE3_12._

object EXERCISE3_13 extends App{
  // It has time complexity O(2N) , better than the usual O(N^2)
  def foldRightInTermsOfFoldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    val reversedList = reverseList(as)
    foldLeft(reversedList, z)(f)
  }
}

