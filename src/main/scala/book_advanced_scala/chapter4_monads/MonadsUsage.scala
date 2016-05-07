package book_advanced_scala.chapter4_monads

object MonadsUsage {
  import cats.Monad
  // import cats.Monad
  import cats.std.option._
  // import cats.std.option._
  import cats.std.list._
  // import cats.std.list._
  val opt1 = Monad[Option].pure(3) // opt1: Option[Int] = Some(3)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2)) // opt2: Option[Int] = Some(5)
  val list1 = Monad[List].pure(3) // list1: List[Int] = List(3)
  val list2 = List(1, 2, 3)
  // list2: List[Int] = List(1, 2, 3)
  val list3 = Monad[List].flatMap(list2)(x => List(x, x*10)) // list3: List[Int] = List(1, 10, 2, 20, 3, 30)

  val tupled: Option[(Int, String, Double)] = Monad[Option].tuple3(Option(1), Option("hi"), Option(3.0))
  // tupled: Option[(Int, String, Double)] = Some((1,hi,3.0))

  val sequence: Option[List[Int]] = Monad[Option].sequence(List(Option(1), Option(2), Option(3)))
  // sequence: Option[List[Int]] = Some(List(1, 2, 3))
}


