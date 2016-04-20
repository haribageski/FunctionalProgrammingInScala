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

}
