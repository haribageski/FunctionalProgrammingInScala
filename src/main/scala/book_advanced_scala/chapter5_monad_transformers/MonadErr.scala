package book_advanced_scala.chapter5_monad_transformers

import scalaz.{EitherT, MonadError}
import scalaz.std.AllInstances._

object MonadErr {
  type OptionError[E, A] = EitherT[Option, E, A]
  val instance = MonadError[OptionError, String]
  // instance: scalaz.MonadError[OptionError,String] = ...

  // Create a failure using the MonadError itself:
  instance.raiseError[Int]("FAIL!")
  // res0: OptionError[String,Int] = EitherT(Some(-\/(FAIL!)))

  import scalaz.syntax.monadError._
  // Create a failure using the MonadError syntax:
  MonadError[OptionError, String].raiseError[Int]("FAIL!")
  "Nooooo".raiseError[OptionError, Int]
  // res1: OptionError[String,Int] = EitherT(Some(-\/(Nooooo)))
}
