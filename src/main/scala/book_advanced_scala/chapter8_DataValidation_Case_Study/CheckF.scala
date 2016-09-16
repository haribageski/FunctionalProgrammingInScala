package chapter8_DataValidation_Case_Study

import cats.Semigroup
import cats.data.Xor
import cats.syntax.xor._ // For .left and .right
import cats.syntax.semigroup._ // For |+|


case class CheckF[E,A](f: A => E Xor A) {

  import cats.data.Xor._
  // For Left and Right

  def and(that: CheckF[E,A])(implicit s: Semigroup[E]): CheckF[E,A] = {
    val self = this
    CheckF(a =>
      (self(a), that(a)) match {
        case (Left(e1), Left(e2)) => (e1 |+| e2).left
        case (Left(e), Right(a)) => e.left
        case (Right(a), Left(e)) => e.left
        case (Right(a1), Right(a2)) => a.right
      } )
  }

  def apply(a: A): E Xor A =
    f(a)
}
