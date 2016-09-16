package chapter8_DataValidation_Case_Study

import book_advanced_scala.chapter8_DataValidation_Case_Study.CheckError.CheckError
import cats.data.{Kleisli, Validated, Xor}
import cats.data.Validated.{Invalid, Valid}
import chapter8_DataValidation_Case_Study.Check.Check

//Apply CheckError and in case of success map the result using a given f: B => C
object Check{
  import cats.Semigroup
  import cats.data.Validated


  sealed trait Check[E,A,B] {
    def map[C](f: B => C): Check[E,A,C] =
      Map[E,A,B,C](this, f)

    def flatMap[C](f: B => Check[E,A,C]) =
      FlatMap[E,A,B,C](this, f)

    def andThen[C](next: Check[E,B,C]): Check[E,A,C] =
      AndThen[E,A,B,C](this, next)

    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,B]
  }

  object Check {
    def apply[E,A](f: A => Validated[E,A]): Check[E,A,A] = Pure(f)
  }

  final case class Map[E,A,B,C](check: Check[E,A,B], f: B => C) extends Check[E,A,C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,C] =
      check(in) map f   //check(in) == CheckError(in) -> Validated
  }

  final case class FlatMap[E,A,B,C](check: Check[E,A,B], f: B => Check[E,A,C]) extends Check[E,A,C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,C] =
    check(in).withXor {
      _.flatMap (b => f(b)(in).toXor)
    }
  }

  final  case class AndThen[E,A,B,C](check: Check[E,A,B], next: Check[E,B,C]) extends Check[E,A,C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,C] =
      check(in).withXor { _.flatMap (b => next(b).toXor) }
  }

  final case class Pure[E,A,B](f: A => Validated[E,B]) extends Check[E,A,B] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,B] =
    f(in)
  }

  final case class PureCheck[E,A](check: CheckError[E,A]) extends Check[E,A,A] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,A] =
      check(in)     //here happens the usage of CheckError, and inside of it is implemented the basic logic of checking
  }

  def apply[E,A](predicate: CheckError[E,A]): Check[E,A,A] =
    PureCheck(predicate)

  def apply[E,A,B](f: A => Validated[E,B]): Check[E,A,B] =
    Pure(f)

//  sealed trait Check[E,A] {
//    def and(that: Check[E, A]): Check[E, A] =
//      And(this, that)
//
//    def or(that: Check[E, A]): Check[E, A] =
//      Or(this, that)
//
//    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
//      case Pure(f) => f(a)
//      case And(l, r) =>
//        (l(a) |@| r(a)) map { (_, _) => a }
//      case Or(l, r) =>
//        (l(a), r(a)) match {
//          case (Valid(a), _) => Valid(a)
//          case (Invalid(e1), Valid(a)) => Valid(a)
//          case (Invalid(e1), Invalid(e2)) => Invalid(e1 |+| e2)
//        }
//    }
//  }
}
