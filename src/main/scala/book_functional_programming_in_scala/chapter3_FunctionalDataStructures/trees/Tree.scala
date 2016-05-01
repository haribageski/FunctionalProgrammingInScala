package book_functional_programming_in_scala.chapter3_FunctionalDataStructures.trees

sealed trait Tree[+A] {
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
  def maxOfTwoElements[B <% Ordered[B]](x: B, y: B) = {
    if (x < y) y
    else x
  }

  def maxOfThreeElements[B <% Double](x: B, y: B, z: B): B = {
    maxOfTwoElements(maxOfTwoElements(x, y), z)
  }

  def max[B <% Ordered[B]](): B = this match {
    case Leaf(x: B) => x
    case Branch(x: B, left, right) => maxOfThreeElements[B](x, left.max, right.max)
  }

  //EXERCISE3_28
  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(x: A) => Leaf(f(x))
    case Branch(x: A, left, right) => Branch(f(x), left.map(f), right.map(f))
  }

  //EXERCISE3_29
  def fold[B](init: B)(f: (B, A) => B, g: (B, B) => B): B = this match {
    case Leaf(x: A) => f(init, x)
    case Branch(x: A, left: Tree, right: Tree) => g(left.fold(f(init, x))(f, g), right.fold(f(init, x))(f, g))
  }
  def sizeWithFold(): Double = fold(0.0)((acc, tree) => acc + 1, (x, y) => x + y)

  def widthWithFold(): Double = fold(0.0)((acc, treeVal: A) => acc, (leftMax, rightMax) => Math.max(leftMax, rightMax))

  def maxWithFold(): A = this match {
    case Leaf(x) => x
    case Branch(x: A, left, right) => fold(x)(maxOfTwoElements(_, _), maxOfTwoElements(_, _))
  }


  val tree = Branch(3, Branch(2, Leaf(1), Leaf(2)), Leaf(3))
  println(tree.size())
}

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
