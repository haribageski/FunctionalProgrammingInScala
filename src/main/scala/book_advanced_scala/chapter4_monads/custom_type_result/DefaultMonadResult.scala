package book_advanced_scala.chapter4_monads.custom_type_result

import book_advanced_scala.chapter3_functors.{Failure, Result, Success, Warning}
import cats.Monad

object DefaultMonadResult {
  implicit val resultFunctor = new Monad[Result] {
    override def flatMap[A, B](fa: Result[A])(f: (A) => Result[B]): Result[B] = fa match {
      case Success(value) => f(value)
      case Warning(value, msg1) => f(value) match {
        case Success(s) => Warning(s, msg1)
        case Warning(value, msg2) => Warning(value, s"$msg1 \n $msg2")
        case Failure(msg2) => Failure(s"$msg1 \n $msg2")
      }
      case Failure(e) => Failure(e)
    }

    override def pure[A](x: A): Result[A] = Success(x)
  }
}
