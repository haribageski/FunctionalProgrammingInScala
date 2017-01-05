package book_functional_programming_in_scala.chapter8_PropertyBasedTesting

import book_functional_programming_in_scala.chapter3_FunctionalDataStructures.trees.{Leaf, Tree, Branch}

case class SGen[A](forSize: Int => Gen[A]) {

  def apply(n: Int): SGen[A] =
    SGen(n => forSize(n))

  def map[B](f: A => B): SGen[B] =
    SGen(n => Gen(forSize(n).sample.map(f)))

  def flatMap[B](f: A => SGen[B]) =
    SGen(n => forSize(n).flatMap[B](f(_).forSize(n)))

  def **[B](s2: SGen[B]): SGen[(A,B)] =
    SGen(
      n =>
        s2.forSize(n).flatMap[(A, B)](b =>
          Gen(forSize(n).sample.map[(A, B)](a => (a, b)))
        )
    )
}
object SGen {
  import Gen._

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(n, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(Math.max(1, n), g))

  def tree[A](g: Gen[A]): SGen[Tree[A]] = {
    SGen {
      case i if i <= 1 => g.map[Tree[A]](a => Leaf(a))
      case i => Gen.choose(0, i).flatMap[Tree[A]](sizeOfLeft =>
        tree(g).forSize(sizeOfLeft).flatMap[Tree[A]](leftTree =>
          tree(g).forSize(i - sizeOfLeft).flatMap[Tree[A]](rightTree =>
            g.map[Tree[A]](Branch(_, leftTree, rightTree))
          )
        )
      )
    }
  }
}
