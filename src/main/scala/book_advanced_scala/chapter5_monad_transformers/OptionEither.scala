package book_advanced_scala.chapter5_monad_transformers

import scalaz.{-\/, EitherT}
import scalaz.std.AllInstances._
object OptionEither {

  val error: EitherT[Option, String, Int] = EitherT.left[Option, String, Int](Some("Badness"))
  val sameError: EitherT[Option, String, Int]  = EitherT[Option, String, Int](Some(-\/("Badness")))
}
