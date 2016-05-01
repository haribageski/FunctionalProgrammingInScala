package book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists

import EXERCISE3_13._
object EXERCISE3_19 extends App{
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    def prependIfPradicateSatisfied(elem: A, list: List[A]) = f(elem) match {
      case true => elem :: list
      case false => list
    }

    foldRightInTermsOfFoldLeft(as, List[A]()) ((acc, elem) => prependIfPradicateSatisfied(elem, acc))
  }
  println(filter(List(1,2,3,4))( _ > 2))

}

