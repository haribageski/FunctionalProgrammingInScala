package book_advanced_scala.chapter4_monads

import cats.Monad

object DefaultInstances {
  import cats.std.option._
  // import cats.std.option._
  Monad[Option].flatMap(Option(1))(x => Option(x*2)) // res0: Option[Int] = Some(2)
  import cats.std.list._
  // import cats.std.list._
  Monad[List].flatMap(List(1, 2, 3))(x => List(x, x*10)) // res1: List[Int] = List(1, 10, 2, 20, 3, 30)
  import cats.std.vector._
  // import cats.std.vector._
  Monad[Vector].flatMap(Vector(1, 2, 3))(x => Vector(x, x*10)) // res2: Vector[Int] = Vector(1, 10, 2, 20, 3, 30)
}
