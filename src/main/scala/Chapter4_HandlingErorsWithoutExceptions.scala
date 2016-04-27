import scala.Option

object Chapter4_HandlingErorsWithoutExceptions {
  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(x) => Some(f(x))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(x) => x
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(x) => f(x)
      case _ => None
    }


    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case x: Any => x
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(x) =>
        if(f(x)) Some(x)
        else None
      case _ => None
    }
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]


  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aVal => b.map(f(aVal, _)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @scala.annotation.tailrec
    def iterateTailRec(aList: List[Option[A]], returningList: Option[List[A]]): Option[List[A]] =
      aList match {
        case h :: t   if(h != None)  => iterateTailRec(t, returningList.flatMap(list => h.map(_ :: list)))
        case Nil => returningList.map(_.reverse)
      }
    iterateTailRec(a, Some(List[A]()))
  }


  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @scala.annotation.tailrec
    def iterateTailRec(aList: List[A], resultListO: Option[List[B]]): Option[List[B]] = {
      aList match {
        case h :: t =>
          val mappedVal: Option[B] = f(h)
          mappedVal match {
            case Some(x) => iterateTailRec(t, resultListO.map(x :: _))
          }
        case Nil => resultListO.map(_.reverse)
      }
    }
    iterateTailRec(a, Some(List[B]()))
  }




  sealed trait Either[+E,+A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(x) => Right(f(x))
      case Left(x) => Left(x)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(x) => f(x)
      case Left(x) => Left(x)
    }

    def orElse[EE >: E, B >: A](b: => Eith`er[EE, B]): Either[EE, B] = this match {
      case Right(x) => Right(x)
      case Left(x) => b
    }

  }
  case class Left[+E](get: E) extends Either[E,Nothing]
  case class Right[+A](get: A) extends Either[Nothing,A]
}
