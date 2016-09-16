package book_advanced_scala.chapter8_DataValidation_Case_Study

import book_advanced_scala.chapter8_DataValidation_Case_Study.CheckError.CheckError.{And, Or, Pure}
import cats.data.Validated.{Invalid, Valid}
import cats.data.{Kleisli, Xor}

//If there are errors handle them , otherwise return the same value of Success
object CheckError{
  import cats.Semigroup
  import cats.data.Validated
  import cats.syntax.semigroup._ // For |+|
  import cats.syntax.cartesian._ // For |@|

  sealed trait CheckError[E,A] {
    def and(that: CheckError[E, A]): CheckError[E, A] =
      And(this, that)

    def or(that: CheckError[E, A]): CheckError[E, A] =
      Or(this, that)

    def run(implicit s: Semigroup[E]): A => Xor[E,A] =
      (a: A) => this.apply(a).toXor

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
      case Pure(f) => f(a)
      case And(l, r) =>
        (l(a) |@| r(a)) map { (_, _) => a }
      case Or(l, r) =>
        (l(a), r(a)) match {
          case (Valid(a), _) => Valid(a)
          case (Invalid(e1), Valid(a)) => Valid(a)
          case (Invalid(e1), Invalid(e2)) => Invalid(e1 |+| e2)
        }
    }
  }

  object CheckError {

    final case class Pure[E, A](f: A => Validated[E, A]) extends CheckError[E, A]

    final case class And[E, A](left: CheckError[E, A], right: CheckError[E, A]) extends CheckError[E, A]

    final case class Or[E, A](left: CheckError[E, A], right: CheckError[E, A]) extends CheckError[E, A]

    def apply[E, A](f: A => Validated[E, A]): CheckError[E, A] = Pure(f)

    def lift[E, A](msg: E)(pred: A => Boolean): CheckError[E, A] = Pure { (a: A) =>
      if (pred(a)) Validated.valid(a)
      else
        Validated.invalid(msg)
    }
  }
}
