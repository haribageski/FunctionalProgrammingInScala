package book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists

object EXERCISE3_8 extends App{
  val list = List(1,2,3).foldRight(Nil:List[Int])((list, acc) => list :: acc)
  println(list)   //the initial list, as expected
}
