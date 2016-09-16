package book_advanced_scala.chapter5_monad_transformers

import scalaz.{EitherT, \/, \/-}
import scalaz.std.AllInstances._
import scalaz.syntax.monad._

object Unpacking {

  type OptionEither[A] = EitherT[Option, String, A]
  val packed: OptionEither[Int] = 42.point[OptionEither]
  val unpacked: Option[\/[String, Int]] = packed.run
  val unpacked2: Option[scalaz.\/[String, Int]] = Some(\/-(42))
}
