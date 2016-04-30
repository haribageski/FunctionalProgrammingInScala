package book_advanced_scala.chapter3_functors

import cats.Functor

object DefaultFunctors {
  implicit val resultFunctor = new Functor[Result] {
    override def map[A, B](fa: Result[A])(f: A => B): Result[B] = fa match {
      case Success(value) => Success(f(value))
      case Warning(value, message) => Warning(f(value),message)
      case Failure(message) => Failure(message)
    }
  }
}
