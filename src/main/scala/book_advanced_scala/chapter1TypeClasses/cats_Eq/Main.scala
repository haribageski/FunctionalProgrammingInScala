package book_advanced_scala.chapter1TypeClasses.cats_Eq

import cats.syntax.eq._
import EqDefaults._

object Main extends App{
  val cat1 = Cat("Mark", 2, "wight")
  val cat2 = Cat("Bobi", 3, "black")
  println(cat1 === cat2)
  println(cat1 === cat1)
  println(cat1 =!= cat2)
}
