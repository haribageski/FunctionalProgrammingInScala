import scala.Stream
import scala.collection.immutable.Stream.{#::, cons}

object Chapter5_StrictnessAndLaziness {

  //  trait Stream[A] {
  //    def toList: List[A] = {
  //      def iterateStream(stream: Stream[A], returnedList: List[A]): List[A] = stream match {
  //        case h #:: t =>
  //          val head: Any = h
  //          val tail: Stream[Any] = t
  //          iterateStream(t, h :: returnedList)
  //        case Stream.empty => returnedList.reverse
  //      }
  //    }

  //laziness lets us separate the description of an expression from the evaluation of that expression.
  trait MyStream[A] extends Stream[A] {
    def existsNonLazy(p: A => Boolean): Boolean = this match {
      case h #:: t =>
        println(h)
        p(h) || t.exists(p)
      case _ => false
    }


    def existsLazy(p: A => Boolean): Boolean = {
      def applyLazyPredicate(p: A => Boolean, value: => Stream[A]): Boolean = value match {
        case h #:: t =>
          println(h)
          p(h) || applyLazyPredicate(p, t)
        case _ => false
      }
      applyLazyPredicate(p, this)
    }
  }


  }

}
