package book_advanced_scala.chapter2_monoids

import cats.Monoid
import cats.syntax.semigroup._
import cats.std.int._

import AdditionSyntax._

object Monoids extends App{
  import DefaultMonoidsAddition._

  def add(items: List[Option[Int]]): Int = {
    items.foldLeft(Monoid[Int].empty)((acc, elemO) => elemO.map(elem => elem |+| acc).getOrElse(acc))
  }


  def addInts(items: List[Int]) =
    AdditionOps(items).add

  def addOrders(items: List[Order]) =
    AdditionOps(items).add

  def addOrders(items: Seq[Order]) =
    AdditionOps(items).add


  println(addInts(List(1, 5, 3)))
  println(addOrders(List(Order(1,2), Order(2, 5))))
  println(addOrders(Seq(Order(1,2), Order(2, 5))))
}


case class Order(totalCost: Double, quantity: Double)
