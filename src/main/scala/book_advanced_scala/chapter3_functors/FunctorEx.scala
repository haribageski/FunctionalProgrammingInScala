package book_advanced_scala.chapter3_functors

import DefaultFunctors._
import FunctorSyntax._
import cats.Functor

object FunctorEx extends App{
  def success[A](value: A): Result[A] = Success(value)
  // success: [A](value: A)Result[A]


  val x = Success(100).map(_ * 2)
}

sealed trait Result[+A] // defined trait Result
final case class Success[A](value: A) extends Result[A] // defined class Success
final case class Warning[A](value: A, message: String) extends Result[A] // defined class Warning
final case class Failure(message: String) extends Result[Nothing] // defined class Failure
