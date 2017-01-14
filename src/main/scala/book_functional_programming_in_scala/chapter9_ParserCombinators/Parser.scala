package book_functional_programming_in_scala.chapter9_ParserCombinators

import book_functional_programming_in_scala.chapter9_ParserCombinators.Errors.{Location, ParserErrors}
import book_functional_programming_in_scala.chapter9_ParserCombinators.Parser.Result


case class Parser[+A](run: Location => Result[A])

object Parser {
  type InspectedInput = String
  sealed trait Result[+A] {
    def mapError(f: ParserErrors => ParserErrors): Result[A] = this match {
      case Success(get, charsConsumed) => Success(get, charsConsumed)
      case Failure(get, isCommitted) =>    Failure(f(get), isCommitted)
    }
    def uncommit: Result[A] = this match {
      case _: Success[A] => this
      case Failure(get, _) =>    Failure(get, false)
    }
    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e,c) => Failure(e, c || isCommitted)
      case _ => this
    }
    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _ => this
    }
    def backwardSuccess(n: Int): Result[A] = this match {
      case Success(a, m) =>
        if(m >= n)  Success(a, m - n)
        else Failure(ParserErrors().push(Location(m, "location backward by " + n), "n <= m"), false)
      case _ => this
    }
    def onFailure[B >: A] (result: Result[B]) = this match {
      case f: Failure => result
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParserErrors, isCommitted: Boolean) extends Result[Nothing]

}
