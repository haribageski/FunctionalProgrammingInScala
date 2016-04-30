package book_advanced_scala.chapter4_monads

object MonadIsFunctor {
  import scala.language.higherKinds
  def flatMap[F[_], A, B](value: F[A])(func: A => F[B]): F[B] = ???
  def pure[F[_], A](value: A): F[A] = ???

  def map[F[_], A, B](fa: F[A])(f: A => B): F[B] = {
    flatMap(fa)(pure(f))
  }
//  def map[A, B](fa: F[A])(f: A => B): F[B]
}
