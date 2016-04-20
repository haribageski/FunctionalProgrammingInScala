import Option

object Chapter4_HandlingErorsWithoutExceptions {
  case class MyOption[+A](xOpt: Option[A]) {

    def map[B](f: A => B): Option[B] = xOpt match {
      case None => None
      case Some(x) => Some(f(x))
    }

    def getOrElse[B >: A](default: => B): B = xOpt match {
      case None => default
      case Some(x) => x
    }

    def flatMap[B](f: A => Option[B]): Option[B] = xOpt match {
      case Some(x) => f(x)
      case _ => None
    }


    def orElse[B >: A](ob: => Option[B]): Option[B] = xOpt match {
      case None => ob
      case x: Any => x
    }

    def filter(f: A => Boolean): Option[A] = xOpt match {
      case Some(x) =>
        if(f(x)) Some(x)
        else None
      case _ => None
    }
  }

  def variance(xs: Seq[Double]): Option[Double] =  {
      def findMean: Option[Double] = xs.size match {
        case 0 => None
        case x => Some(xs.sum / x)
      }
    findMean.flatMap(Math.pow())
    mean
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(aVal => b.map(f(aVal, _)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      def iterateTailRec(aList:  List[Option[A]], returningList: Option[List[A]]): Option[List[A]] = aList match {
        case h :: t => iterateTailRec(t, returningList.flatMap(list => h.map(_ :: list)))
        case Nil => returningList.map(_.reverse)
      }
    iterateTailRec(a, Some(List[A]()))
  }

}
