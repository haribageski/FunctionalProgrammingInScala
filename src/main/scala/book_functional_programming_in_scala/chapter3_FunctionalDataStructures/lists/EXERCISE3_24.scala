package book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists

object EXERCISE3_24 extends App{
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @scala.annotation.tailrec
    def hasSubsequenceRecursive(supPartial: List[A], subPartial: List[A]): Boolean = (supPartial, subPartial) match {
      case (Nil, _) => false
      case (_, Nil) => true
      case _ =>
        if(supPartial.head == subPartial.head)
          hasSubsequenceRecursive(supPartial.tail, subPartial.tail)
        else
          hasSubsequenceRecursive(supPartial.tail, sub)
    }
    hasSubsequenceRecursive(sup, sub)
  }
}

