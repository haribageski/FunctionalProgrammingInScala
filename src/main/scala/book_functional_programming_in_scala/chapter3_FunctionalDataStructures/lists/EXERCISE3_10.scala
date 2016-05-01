package book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists

object EXERCISE3_10 extends App{
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], acc: B)(f: (B, A) => B): B = as match {
    case Nil => acc
    case _ => foldLeft(as.tail, f(acc, as.head))(f)
  }
  println("ex3_10FoldLeft:" + foldLeft(List(1,2,3), List[Int]())((acc, el)  => el :: acc))
}

