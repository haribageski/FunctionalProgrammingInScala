package book_functional_programming_in_scala.chapter9_ParserCombinators

import book_functional_programming_in_scala.chapter9_ParserCombinators.Errors.{ParseState, ParserErrors}
import book_functional_programming_in_scala.chapter9_ParserCombinators.Parser.Result


case class Parser[+A](run: ParseState => Result[A])

object Parser {
  type InspectedInput = String
  sealed trait Result[+A] {
    def msg = this match {
      case Success(get, charsConsumed) => s"Succeed: parsed $get after consuming $charsConsumed characters."
      case Failure(get, isCommitted)   => s"Failed with committed status - $isCommitted.${get.msg}"
    }
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
        else Failure(ParserErrors().push(ParseState(m, "location backward by " + n), "n <= m", true), false)
      case _ => this
    }
    def onFailure[B >: A] (result: Result[B]) = this match {
      case f: Failure => result
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParserErrors, isCommitted: Boolean) extends Result[Nothing]
  case class SliceParsing(inputConsumed: String) extends Result[Nothing]
}
