package book_functional_programming_in_scala.chapter4_HandlingErorsWithoutExceptions.either



object EXERCISE4_6 {
  sealed trait Either[+E,+A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(x) => Right(f(x))
      case Left(x) => Left(x)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(x) => f(x)
      case Left(x) => Left(x)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(x) => Right(x)
      case Left(x) => b
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
      Either[EE, C] = this match {
      case Left(e) => Left(e)
      case Right(x) => b.flatMap((bValf: B) => Right(f(x, bValf)))
    }
  }
  case class Left[+E](get: E) extends Either[E,Nothing]
  case class Right[+A](get: A) extends Either[Nothing,A]
}
