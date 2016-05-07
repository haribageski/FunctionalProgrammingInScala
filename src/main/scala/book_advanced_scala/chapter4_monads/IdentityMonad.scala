package book_advanced_scala.chapter4_monads

object IdentityMonad extends App{
  import cats.Id
  import cats.syntax.flatMap._

  val a: Id[Int] = 3
  val b: Id[Int] = a.flatMap(_ + 2) // b: cats.Id[Int] = 5
  println(b)
  val c: Id[Int] = a + 2 // c: cats.Id[Int] = 5
}
