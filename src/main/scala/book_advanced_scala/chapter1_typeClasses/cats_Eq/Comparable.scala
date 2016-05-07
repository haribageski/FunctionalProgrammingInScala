package book_advanced_scala.chapter1_typeClasses.cats_Eq

import cats.Eq
import cats.syntax.eq._

final case class Cat(name: String, age: Int, color: String)


object EqDefaults {
  implicit val eqCat: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
    import cats.std.int._
    import cats.std.string._

    (cat1.age === cat2.age) &&
      (cat1.name === cat2.name) &&
      (cat1.color === cat2.color)
  }
}
