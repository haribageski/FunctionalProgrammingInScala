package book_functional_programming_in_scala.chapter9_ParserCombinators

import book_functional_programming_in_scala.chapter9_ParserCombinators.Errors.{Committed, CommittedStatusOfError, ParserErrors}
import book_functional_programming_in_scala.chapter9_ParserCombinators.Parser.InspectedInput

case class Parser[+A](
                       run: String => Either[ParserErrors, (A, InspectedInput)],   //Error
                       committedStatusOfError: CommittedStatusOfError = Committed
                     ) {
  def result(input: String): Either[ParserErrors, A] = run(input).fold[Either[ParserErrors, A]](e => Left(e), {
    case (a: A, inspectedInput: InspectedInput) => Right(a)
  })
}

object Parser {
  type InspectedInput = String
}
