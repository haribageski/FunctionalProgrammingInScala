package book_functional_programming_in_scala.chapter2_GettingStarted

object EXERCISE2_2 {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def compareTwoElementsAtIndex(index: Int): Boolean = {
      if(index > as.length) true
      else  if(!ordered(as(index - 1) , as(index)))   false
      else  compareTwoElementsAtIndex(index + 1)
    }
    compareTwoElementsAtIndex(1)
  }
}
