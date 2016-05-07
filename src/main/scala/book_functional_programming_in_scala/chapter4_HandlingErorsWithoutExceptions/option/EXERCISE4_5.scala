package book_functional_programming_in_scala.chapter4_HandlingErorsWithoutExceptions.option

object EXERCISE4_5 {

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @scala.annotation.tailrec
    def iterateTailRec(aList: List[A], resultListO: Option[List[B]]): Option[List[B]] = {
      aList match {
        case h :: t =>
          val mappedVal: Option[B] = f(h)
          mappedVal match {
            case Some(x) => iterateTailRec(t, resultListO.map(x :: _))
          }
        case Nil => resultListO.map(_.reverse)
      }
    }
    iterateTailRec(a, Some(List[B]()))
  }

}
