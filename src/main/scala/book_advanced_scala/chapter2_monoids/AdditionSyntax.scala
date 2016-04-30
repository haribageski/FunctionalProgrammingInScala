package book_advanced_scala.chapter2_monoids

import cats.Monoid
import cats.syntax.semigroup._
import scala.language.higherKinds

object AdditionSyntax {
  implicit class AdditionOps[A,F[A] <: Traversable[A]](classVal: F[A]){
    def add(implicit monoid: Monoid[A]) = {
      classVal.foldLeft(monoid.empty)(_ |+| _)    // |+| comes from cats.syntax.semigroup
    }
  }
}
