package book_functional_programming_in_scala.chapter3_FunctionalDataStructures.trees

class EXERCISE3_25[A](tree:Tree[A]) extends App{
  def getVal: A = tree match {
    case Leaf(x) => x
    case Branch(x, left, right) => x
  }

  def size(): Int = tree match {
    case Leaf(x) => 1
    case Branch(x, left, right) => 1 + size(left) + size(right)
  }
}

