package book_functional_programming_in_scala.chapter6_PurelyFunctionalState

//State is short for computation that carries some state along, or state action, state transition, or even statement
case class State[S,+A](run: S => (A,S)) {
  import State._

  def flatMap[B](g: A => State[S, B]): State[S, B] = {
    State {
      s: S =>
        val (a: A, rngNext: S) = run(s)
        g(a).run(rngNext)
    }
  }

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](s: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => s.map(b => f(a, b)))
}



object State {
  def unit[A, S](a: A): State[S, A] = {
    State {
      s: S => (a, s)
    }
  }

  def elemAndList[A, S](s: State[S, A], list: State[S, List[A]]): State[S, List[A]] =
    s.map2(list)(_ :: _)

  def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] = {
    @scala.annotation.tailrec
    def iterate(fs: List[State[S, A]], acc: State[S, List[A]]): State[S, List[A]] = fs match {
      case Nil =>
        State {
          s: S =>
            val (list, rngLast) = acc.run(s)
            (list.reverse, rngLast)
        }
      case h :: t =>
        iterate(t, elemAndList(h, acc))
    }
    iterate(fs, unit(Nil))
  }
}
