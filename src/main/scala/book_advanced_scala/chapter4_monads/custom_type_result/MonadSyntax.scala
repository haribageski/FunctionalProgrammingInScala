package book_advanced_scala.chapter4_monads.custom_type_result

import cats.Monad

object MonadSyntax {
  implicit class FunctorResult[A, R[_]](value: R[A]) {
    def flatMap[B](f: A => R[B])(implicit monad: Monad[R]): R[B] =
      monad.flatMap(value)(f)

    def map[B](f: A => B)(implicit monad: Monad[R]): R[B] =
      monad.map(value)(f)
  }
}
