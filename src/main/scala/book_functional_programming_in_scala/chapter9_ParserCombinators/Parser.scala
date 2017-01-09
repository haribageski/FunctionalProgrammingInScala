package book_functional_programming_in_scala.chapter9_ParserCombinators

import book_functional_programming_in_scala.chapter9_ParserCombinators.Errors.{Committed, CommittedStatusOfError, KnownLocation, Location, ParserError, ParserErrors}
import book_functional_programming_in_scala.chapter9_ParserCombinators.Parser.{Failure, InspectedInput, Result, Success}


case class Parser[+A](
                       run: Location => Result[A],   //Error
                       committedStatusOfError: CommittedStatusOfError = Committed
                     ) {
  def result(input: String, from: Int): Either[ParserErrors, A] = run(KnownLocation(from, input)) match {
    case Success(get, charsConsumed) => Right(get)
    case Failure(get) => Left(get)
  }

}

object Parser {
  type InspectedInput = String
  sealed trait Result[+A] {
    def mapError(f: ParserErrors => ParserErrors): Result[A] = this match {
      case Success(get, charsConsumed) => Success(get, charsConsumed)
      case Failure(get) =>    Failure(f(get))
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParserErrors) extends Result[Nothing]

}
