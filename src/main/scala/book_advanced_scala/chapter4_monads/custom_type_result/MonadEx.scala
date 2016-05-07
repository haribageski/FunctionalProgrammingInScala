package book_advanced_scala.chapter4_monads.custom_type_result

import book_advanced_scala.chapter3_functors.{Failure, Result, Success, Warning}
import MonadSyntax._
import book_advanced_scala.chapter4_monads.custom_type_result.DefaultMonadResult._

object MonadEx extends App{
  def warning[A](value: A, msg: String): Result[A] = Warning(value, msg)
  def success[A](value: A): Result[A] = Success(value)
  def failure(msg: String): Result[Nothing] = Failure(msg)

  val res1 = warning(100, "warning message 1").map(_ * 2)
  val res2 = warning(100, "warning message 1").flatMap(x => Warning(x * 2, "warning message 2"))
  val res3 = warning(100, "warning message 1").flatMap(x => Success(x * 2))
  val res4 = warning(100, "warning message 1").flatMap(x => Failure( "warning message 2"))
  val res5 = success("warning message 1").flatMap(x => Warning(x * 2, "warning message 2"))
  val res6 = failure("failure message 1").flatMap((x: Nothing) => Failure("failure message 2"))  //TODO: must we set the type of x to Nothing

  println(res1)
  println(res2)
  println(res3)
  println(res4)
  println(res5)
  println(res6)
}

    // defined class Failure
