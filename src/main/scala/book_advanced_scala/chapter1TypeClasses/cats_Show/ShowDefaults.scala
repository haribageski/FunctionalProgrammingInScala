package book_advanced_scala.chapter1TypeClasses.cats_Show

import cats.Show

final case class Cat(name: String, age: Int, color: String)

object ShowDefaults {
  implicit def valueShow: Show[Cat] = Show.show[Cat] { cat =>
    s"${cat.name}is a ${cat.age} year-old ${cat.color} cat."
  }
}


