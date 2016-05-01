package book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists

object EXERCISE3_4 {
  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case h :: t =>
      if(n > 0)
        drop(t, n-1)
      else
        t
  }
}
