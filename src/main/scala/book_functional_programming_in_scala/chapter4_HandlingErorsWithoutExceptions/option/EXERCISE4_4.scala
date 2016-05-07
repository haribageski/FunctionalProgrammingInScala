package book_functional_programming_in_scala.chapter4_HandlingErorsWithoutExceptions.option

object EXERCISE4_4 {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @scala.annotation.tailrec
    def iterateTailRec(aList: List[Option[A]], returningList: Option[List[A]]): Option[List[A]] =
      aList match {
        case h :: t   if(h != None)  => iterateTailRec(t, returningList.flatMap(list => h.map(_ :: list)))
        case Nil => returningList.map(_.reverse)
      }
    iterateTailRec(a, Some(List[A]()))
  }
}
