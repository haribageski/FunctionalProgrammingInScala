package book_advanced_scala.chapter3_functors

import cats.Functor

object FunctorSyntax {
  implicit class FunctorResult[A, R[_]](value: R[A]) {
    def map[B](f: A => B)(implicit functor: Functor[R]): R[B] =
      functor.map(value)(f)
  }
}
