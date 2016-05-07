package book_functional_programming_in_scala.chapter4_HandlingErorsWithoutExceptions.option

object EXERCISE4_1 {
  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(x) => Some(f(x))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(x) => f(x)
      case _ => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(x) => x
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case x: Any => x
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(x) =>
        if(f(x)) Some(x)
        else None
      case _ => None
    }
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

}
