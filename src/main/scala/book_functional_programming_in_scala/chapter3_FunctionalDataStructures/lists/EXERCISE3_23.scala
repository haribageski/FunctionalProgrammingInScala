package book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists

import book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists.EXERCISE3_12._

object EXERCISE3_23 extends App{
  def zipWith[A](listA: List[A], listB: List[A])(f: (A, A) => A): List[A] = {

    @annotation.tailrec
    def zipAccumulated(listA: List[A], listB: List[A], listAccumulated: List[A]): List[A] = (listA, listB) match {
      case (Nil, _) => listAccumulated
      case (_, Nil) => listAccumulated
      case _ => zipAccumulated(listA.tail, listB.tail, f(listA.head, listB.head) :: listAccumulated)   //putting in reverse order intendedly
    }
    zipAccumulated(reverseList(listA), reverseList(listB), List.empty[A])   //start with reversed lists because the implementation works in reverse order
  }

  println("ex3_23ZipWith:" + zipWith(List(1,2,3), List(2,3,4))(_ + _))
}

