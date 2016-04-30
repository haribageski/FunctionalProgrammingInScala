package book_functional_programming_in_scala

import scala.annotation.tailrec

object Chapter3_FunctionalDataStructures  {
  def ex3_7_productEfficientRecursion[A <% Double](list: List[A]): Double = {
    list.foldRight(1.0)((left, acc) => left match {
      case 0 => return 0
      case _ =>
        println(left)
        left * acc
    })
  }

  def ex3_9: Unit = {
    def length[A](as: List[A]): Int = {
      as.foldRight(0)((elem, acc) => acc + 1)
    }
    println(length(List(1, 2, 3, 4)))
  }

  @annotation.tailrec
  def ex3_10FoldLeft[A, B](as: List[A], acc: B)(f: (B, A) => B): B = as match {
    case Nil => acc
    case _ => ex3_10FoldLeft(as.tail, f(acc, as.head))(f)
  }

  def ex3_12ReverseList[A](as: List[A]): List[A] = {
    ex3_10FoldLeft(as, List[A]())((acc, elem) => elem :: acc)
  }

  def ex3_13FoldRightInTermsOfFoldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    val reversedList = ex3_12ReverseList(as)
    ex3_10FoldLeft(reversedList, z)(f)
  }

  def ex3_18_Map[A,B](as: List[A])(f: A => B): List[B] = {
    ex3_13FoldRightInTermsOfFoldLeft[A,List[B]](as, List[B]())((acc, elem) => f(elem) :: acc)
  }

  def ex3_19_Filter[A](as: List[A])(f: A => Boolean): List[A] = {
    def prependIfPradicateSatisfied(elem: A, list: List[A]) = f(elem) match {
      case true => elem :: list
      case false => list
    }

    ex3_13FoldRightInTermsOfFoldLeft(as, List[A]()) ((acc, elem) => prependIfPradicateSatisfied(elem, acc))
  }

  def ex3_20_FlatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    ex3_13FoldRightInTermsOfFoldLeft(as, List[B]())((acc, elem) => f(elem) ::: acc)
  }

  def ex3_23ZipWith[A](listA: List[A], listB: List[A])(f: (A, A) => A): List[A] = {

    @annotation.tailrec
    def zipAccumulated(listA: List[A], listB: List[A], listAccumulated: List[A]): List[A] = (listA, listB) match {
      case (Nil, _) => listAccumulated
      case (_, Nil) => listAccumulated
      case _ => zipAccumulated(listA.tail, listB.tail, f(listA.head, listB.head) :: listAccumulated)
    }

    zipAccumulated(ex3_12ReverseList(listA), ex3_12ReverseList(listB), List[A]())
  }

  def ex3_24HasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def hasSubsequenceRecursive(supPartial: List[A], subPartial: List[A]): Boolean = (supPartial, subPartial) match {
      case (Nil, _) => false
      case (_, Nil) => true
      case _ =>
        if(supPartial.head == subPartial.head)
          hasSubsequenceRecursive(supPartial.tail, subPartial.tail)
        else
          hasSubsequenceRecursive(supPartial.tail, sub)
    }
    hasSubsequenceRecursive(sup, sub)
  }


  val listA = List(1, 3, 5, 0.0, 4, 6)
  val product = ex3_7_productEfficientRecursion(listA)
  println("resulting product:" + product)
  println("ex3_10FoldLeft:" + ex3_10FoldLeft(List(1,2,3), List[Int]())((acc, el)  => el :: acc))
  println("ex3_12ReverseList:" + ex3_12ReverseList(List(1, 2, 3)))
  println("ex3_18_map:" + ex3_18_Map(List(1,2,3))(_.toString))
  println(ex3_19_Filter(List(1,2,3,4))( _ > 2))

  println("ex3_23ZipWith:" + ex3_23ZipWith(List(1,2,3), List(2,3,4))(_ + _))
}
