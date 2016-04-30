package book_functional_programming_in_scala

import Stream._

import scala.collection.mutable.ListBuffer
trait Stream[+A] {

  // The natural recursive solution
  def toListRecursive: List[A] = this match {
    case Cons(h,t) => h() :: t().toListRecursive
    case _ => List()
  }


//  def toListSlow: List[A] = {
//    @scala.annotation.tailrec
//    def convert(stream: => Stream[A], acc: List[A]): List[A] = stream match {
//      case (h, t) => convert(t, h :: acc)
//      case _ => acc
//    }
//    convert(this, List.empty[A]).reverse
//  }


  def toListFast: List[A] = {
    @scala.annotation.tailrec
    def convert(stream: Stream[A], acc: ListBuffer[A]): List[A] = stream match {
      case Cons(h,t)=> convert(t(), acc += h())
      case _ => acc.toList
    }
    convert(this, ListBuffer.empty[A])
  }


  def takeN(n: Int) = {
    def takeFromStream(stream: => Stream[A], n: Int, acc: ListBuffer[A]): List[A] = {
      if (n > 0)
        this match {
          case Cons(h,t) => takeFromStream(t(), n - 1, acc += h())
          case _ => acc.toList
        }
      else
        acc.toList
    }
    takeFromStream(this, n, ListBuffer.empty)
  }


//  def existsLazy(p: A => Boolean): Boolean = {
//    @scala.annotation.tailrec
//    def applyLazyPredicate(p: A => Boolean, value: => Stream[A]): Boolean = value match {
//      case (h, t) =>
//        println(h)
//        p(h) || applyLazyPredicate(p, t)
//      case _ => false
//    }
//    applyLazyPredicate(p, this)
//  }


//  def foldRightLazy[B](z: => B)(f: (A, => B) => B): B = {
//    this match {
//      case (h, t) =>
//        f(h, t.foldRightLazy(z)(f))
//      case _ => z
//    }
//  }
//
//  takeWhileLazy(p: A => Boolean) = {
//    foldRightLazy[Stream[A]](MyStream)()
//  }
}



case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}

