package book_advanced_scala.chapter4_monads
import cats.data.Xor

object xorMonad extends App{
  import cats.syntax.xor._

  val a: Xor[Nothing, Int] = 3.right
  val b: Xor[Nothing, Int] = 4.right
  val s: Xor[String, Nothing] = "4".left

  val p = for {
    x <- a
    y <- b
  } yield x + y

  println(p)

  val z = 123.right[String]
  // a: cats.data.Xor[String,Int] = Right(123)
  val t = a.swap
  // b: cats.data.Xor[Int,String] = Left(123)
}
