package book_functional_programming_in_scala.chapter3_FunctionalDataStructures.trees

sealed trait Tree[A] {
  def getVal: A = this match {
    case Leaf(x) => x
    case Branch(x, left, right) => x
  }

  //EXERCISE3_25
  def size(): Int = this match {
    case Leaf(x) => 1
    case Branch(x, left, right) => 1 + left.size() + right.size()
  }

  //EXERCISE3_26
  def maxOfTwoElements[A](x: A, y: A)(implicit ordering: Ordering[A]): A = {
    if (ordering.lt(x, y))  y
    else x
  }

  def maxOfThreeElements[A](x: A, y: A, z: A)(implicit ordering: Ordering[A]): A = {
    maxOfTwoElements(maxOfTwoElements(x, y), z)
  }

  def max(implicit ordering: Ordering[A]): A = this match {
    case Leaf(x) => x
    case Branch(x, left, right) => maxOfThreeElements(x, left.max, right.max)
  }

  //EXERCISE3_28
  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(x: A) => Leaf(f(x))
    case Branch(x: A, left, right) => Branch(f(x), left.map(f), right.map(f))
  }

  //EXERCISE3_29
  def foldTree[B](init: B)(f: (B, A) => B, g: (B, B) => B): B = this match {
    case Leaf(x: A) => f(init, x)
    case Branch(x: A, left, right) => g(left.foldTree(f(init, x))(f, g), right.foldTree(f(init, x))(f, g))
  }
  def sizeWithFold(implicit ordering: Ordering[A]): Double = foldTree(0.0)((acc, tree) => acc + 1, (x, y) => x + y)

  def widthWithFold(implicit ordering: Ordering[A]): Double = foldTree(0.0)((acc, treeVal) => acc + 1, (leftMax, rightMax) => Math.max(leftMax, rightMax))

//  def maxWithFold[A](implicit ordering: Ordering[A]): A = this match {
//    case Leaf(x: A) => x
//    case Branch(x: A, left, right) => this.foldTree[A](x)(maxOfTwoElements, maxOfTwoElements(_, _))
//  }
}

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
