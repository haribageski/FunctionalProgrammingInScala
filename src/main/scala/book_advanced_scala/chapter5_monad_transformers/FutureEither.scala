package book_advanced_scala.chapter5_monad_transformers

import scala.concurrent.Future
import scalaz.EitherT


object FutureEither {
  type FutureEither[A] = EitherT[Future,String, A]
}
