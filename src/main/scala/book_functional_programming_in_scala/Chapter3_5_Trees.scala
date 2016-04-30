package book_functional_programming_in_scala

object Chapter3_5_Trees {
  sealed trait Tree[+A] {

    def getVal(): A = this match {
      case Leaf(x: A) => x
      case Branch(x: A, left, right) => x
    }

    def max[B <% Double](x: B , y: B) = {
      if(x < y)  y
      else  x
    }
    def max[B <% Double](x: B , y: B, z: B): B = {
      max(max(x, y), z)
    }

    def size(): Int = this match {
      case Leaf(x) => 1
      case Branch(x, left, right) => 1 + left.size() + right.size()
    }

//    def max[B <% Double](): B = this match {
//      case Leaf(x: B) => x
//      case Branch(x: B, left, right) => max[B](x, left.max, right.max)
//    }

    def map[B](f: A => B): Tree[B] = this match {
      case Leaf(x: A) => Leaf(f(x))
      case Branch(x: A, left, right) => Branch(f(x), left.map(f), right.map(f))
    }

//    def fold[B](init: B)(f: (B, A) => B, g: (B, B) => B): B = this match {
//      case Leaf(x: A) => f(init, x)
//      case Branch(x: A, left: Tree, right: Tree) => g(left.fold(f(init, x))(f, g), right.fold(f(init, x))(f, g))
//    }

//    def sizeWithFold(): Double = fold(0.0)((acc, tree) => acc + 1 , (x, y) => x + y)
//    def widthWithFold(): Double = fold(0.0)((acc, treeVal: A) => Math.max(acc, treeVal),
//      (leftMax, rightMax) => max(leftMax, rightMax))

//    def maxWithFold(): A = fold(0.0)(
//      (acc, tree) =>
//        tree match {
//          case Leaf(x: Double) =>
//            if (acc > x) acc
//            else x
//          case Branch(x: Double, left, right) =>
//            if (acc > x) acc
//            else x
//        },

  }

  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]


  val tree = Branch(3, Branch(2, Leaf(1), Leaf(2)), Leaf(3))
  println(tree.size())
}
