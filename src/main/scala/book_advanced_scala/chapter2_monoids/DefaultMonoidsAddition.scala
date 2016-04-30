package book_advanced_scala.chapter2_monoids

import cats.Monoid
import cats.std.int._

object DefaultMonoidsAddition {
  implicit val monoid: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(0, 0)
    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }
  implicit val monoidInt: Monoid[Int] = Monoid[Int]
}
