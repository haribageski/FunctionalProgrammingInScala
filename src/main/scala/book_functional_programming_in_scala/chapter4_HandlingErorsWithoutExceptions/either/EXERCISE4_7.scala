package book_functional_programming_in_scala.chapter4_HandlingErorsWithoutExceptions.either

import EXERCISE4_6._

object EXERCISE4_7 extends App{
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    @scala.annotation.tailrec
    def tailRecIterate(list: List[Either[E, A]], either: Either[E, List[A]]): Either[E, List[A]] = list match {
      case h :: t =>
        val eitherWithHeadAdded = h.map2(either)((a, list) => a :: list)
        eitherWithHeadAdded match {
          case Left(e) => Left(e)
          case Right(x) => tailRecIterate(t, eitherWithHeadAdded)
        }
      case Nil => either.map(_.reverse)
    }
    tailRecIterate(es, Right(List.empty[A]))
  }

  val e1 = Right(1)
  val e2 = Right(2)
  val e3 = Right(3)
  val e4 = Left("Exception")
  println(sequence(List(e1, e2, e3)))
  println(sequence(List(e1, e2, e3, e4)))
}
