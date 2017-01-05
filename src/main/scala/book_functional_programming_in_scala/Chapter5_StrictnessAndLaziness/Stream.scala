package book_functional_programming_in_scala.chapter5_StrictnessAndLaziness

import book_functional_programming_in_scala.chapter5_StrictnessAndLaziness.Stream._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import NonSharedStateWithUnfold.unfold

trait Stream[+A] {

  // The natural recursive solution
  def toListRecursive: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursive
    case _ => List()
  }

  //O(2N)
  def toListSlow: List[A] = {
    @scala.annotation.tailrec
    def convert(stream: => Stream[A], acc: List[A]): List[A] = stream match {
      case Cons(h, t) => convert(t(), h() :: acc)
      case _ => acc
    }
    convert(this, List.empty[A]).reverse
  }

  //O(N)
  def toListFast: List[A] = {
    @scala.annotation.tailrec
    def convert(stream: => Stream[A], acc: ListBuffer[A]): List[A] = stream match {
      case Cons(h, t) => convert(t(), acc += h())
      case _ => acc.toList
    }
    convert(this, ListBuffer.empty[A])
  }

  def takeN(n: Int): List[A] = {
    @scala.annotation.tailrec
    def takeFromStream(stream: => Stream[A], n: Int, acc: ListBuffer[A]): List[A] = {
      if (n > 0)
        stream match {
          case Cons(h, t) => takeFromStream(t(), n - 1, acc += h())
          case _ => acc.toList
        }
      else
        acc.toList
    }
    takeFromStream(this, n, ListBuffer.empty)
  }

  //  def takeWhile(p: A => Boolean): Stream[A] = {
  //    @scala.annotation.tailrec
  //    def takeFromStream(stream: => Stream[A], acc: Stream[A]): Stream[A] = {
  //      stream match {
  //        case Cons(h, t) =>
  //          if (p(h))
  //            takeFromStream(t(), cons(h(), acc))
  //          else
  //            takeFromStream(t(), acc)
  //        case _ => acc
  //      }
  //    }
  //    takeFromStream(this, Stream.empty)
  //  }

  @scala.annotation.tailrec
  final def existsLazy(p: A => Boolean): Boolean = this match {
    case Cons(h, t) =>
      p(h()) || t().existsLazy(p)
    case _ => false
  }

  @tailrec
  final def forAllLazy(p: A => Boolean): Boolean = this match {
    case Cons(h, t) =>
      p(h()) && t().forAllLazy(p)
    case _ => true

  }

  def foldRightLazy[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) =>
      f(h(), t().foldRightLazy(z)(f))
    case _ => z
  }

  def takeWhileLazy(p: A => Boolean) = {
    foldRightLazy(Stream.empty[A])((elem, stream) =>
      if (p(elem))
        cons(elem, stream)
      else
        Stream.empty[A]
    )
  }
  def headOptionLazy() = {
    foldRightLazy[Option[A]](None)((elem, acc) => Some(elem))
  }

  def mapLazy[B](f: A => B) = {
    foldRightLazy(Stream.empty[B])((elem, acc) => cons(f(elem), acc))
  }

  def filterLazy(p: A => Boolean) = {
    foldRightLazy(Stream.empty[A])((elem, stream) =>
      if (p(elem))
        cons(elem, stream)
      else
        stream
    )
  }

  def appendLazy[B >: A](x: => B) = {
    //we need to make the parapeter to be of type that is supertype of A
    foldRightLazy(Stream(x))(cons(_, _))
  }

  def flatMapLazy[B](f: A => Stream[B]) = {
    foldRightLazy(Stream.empty[B])((elem, acc) =>
      f(elem).foldRightLazy(acc)(cons(_, _))
    )
  }

  def findLazy(p: A => Boolean): Option[A] = filterLazy(p).headOptionLazy



  //Based on Unfold
  def mapWithUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }
  }

  def takeWithUnfold(n: Int) = unfold((this, n))((stateWithIterator: (Stream[A], Int)) =>
    stateWithIterator._1 match {
      case Cons(h, t) => {
        val iter = stateWithIterator._2
        if (iter > 0)
          Some(h(), ((t(), iter - 1)))
        else
          None
      }
      case Empty => None
    })

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) =>
      if (p(h()))
        Some(h(), t())
      else
        None
    case Empty => None
  }

  def zipAllWithUnfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s2))((jointState: (Stream[A], Stream[B])) => {
      jointState._1 match {
        case Cons(h1, t1) => {
          jointState._2 match {
            case Cons(h2, t2) => Some((Some(h1()), Some(h2())), (t1(), t2()))
            case Empty => Some((Some(h1()), None), (t1(), Empty))
          }
        }
        case Empty => {
          jointState._2 match {
            case Cons(h2, t2) => Some((None, Some(h2())), (Empty, t2()))
            case Empty => None
          }
        }
      }
    })
  }

  def startsWith[A](s: Stream[A]): Boolean = {
      zipAllWithUnfold(s)
        .takeWhileWithUnfold(!_._2.isEmpty)
        .forAllLazy(x => x._1 == x._2)
  }

  def tails: Stream[Stream[A]] = unfold(this){
    case Cons(h, t) => Some(t(), t())
    case Empty => None
  }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails.existsLazy(_ startsWith s)


  //Cannot be solved with unfold
  def scanRight[B](z: B)(f: (A, B) => B): List[B] = {
    val combinations: (Stream[B], B) =
      this.foldRightLazy((Stream(z), z))((elem, acc) => (cons(f(elem, acc._2), acc._1), f(elem, acc._2)))
    combinations._1.toListFast
  }

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

  def from(i: Int): Stream[Int] = cons(i, from(i + 1))
}

