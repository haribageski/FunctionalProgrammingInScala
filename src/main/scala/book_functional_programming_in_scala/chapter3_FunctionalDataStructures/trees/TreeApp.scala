package book_functional_programming_in_scala.chapter3_FunctionalDataStructures.trees

import scala.math.Ordering.Int

object TreeApp extends App{
  val tree = Branch(3, Branch(2, Leaf(1), Leaf(2)), Leaf(3))

  println("size:" + tree.size())
  println("sizeWithFold:" + tree.sizeWithFold)
  println("widthWithFold:" + tree.widthWithFold)
//  println("maxWithFold:" + tree.maxWithFold())

}
