package book_advanced_scala.chapter1TypeClasses.cats_Show

import cats.Show

object ShowSyntax {
  implicit class ShowOps[A](value: A) {
    def show(implicit valueShow: Show[A]): String = valueShow.show(value)

    def outputShow(implicit valueShow: Show[A]): Unit = println(valueShow.show(value))
  }
}
