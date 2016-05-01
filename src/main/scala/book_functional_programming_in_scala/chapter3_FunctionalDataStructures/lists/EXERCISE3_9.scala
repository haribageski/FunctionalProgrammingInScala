package book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists

object EXERCISE3_9 extends App{
  def length[A](as: List[A]): Int = {
    as.foldRight(0)((elem, acc) => acc + 1)
  }
  println(length(List(1, 2, 3, 4)))   // 4


  @annotation.tailrec
  def ex3_10FoldLeft[A, B](as: List[A], acc: B)(f: (B, A) => B): B = as match {
    case Nil => acc
    case _ => ex3_10FoldLeft(as.tail, f(acc, as.head))(f)
  }

}

