package book_functional_programming_in_scala.chapter9_ParserCombinators

import book_functional_programming_in_scala.chapter9_ParserCombinators.Errors.{KnownLocation, Location, ParserErrors}
import book_functional_programming_in_scala.chapter9_ParserCombinators.Parser.{Failure, Result, Success}


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
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParserErrors, isCommitted: Boolean) extends Result[Nothing]

}
