package book_functional_programming_in_scala.chapter9_ParserCombinators

import book_functional_programming_in_scala.chapter9_ParserCombinators.Parsers

object Errors {
  type ParserErrorMsg = String

  sealed trait CommittedStatusOfError
  object Committed extends CommittedStatusOfError
  object Uncommitted extends CommittedStatusOfError

  case class Location(location: Int, input: String) {
    require(input.length > location)
    lazy val line = input.slice(0, location + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0,location + 1).lastIndexOf('\n') match {
      case -1 => location + 1
      case lineStart => location - lineStart }
  }

  case class ParserError(msg: ParserErrorMsg)

  case class ParserErrors(errors: List[(Location, ParserErrorMsg)], committedStatusOfError: CommittedStatusOfError = Committed)


  object ParserError {
    trait ReportErrorSide
    case object ReportErrorOfLeftParser extends ReportErrorSide
    case object ReportErrorOfRightParser extends ReportErrorSide
    case object ReportErrorOfBothLeftAndRightParsers extends ReportErrorSide

    def apply(location: Location, expectedInput: String): ParserError = {
      require(expectedInput.length > location.location)
      val l = location.location
      apply(s"Error at position $l: Expected ${expectedInput.charAt(l)} but found ${location.input.charAt(l)}")
    }

    def errorsMsgs[A](p: Parser[A]): ParserErrors = ???   //TODO    implicit parsers: Parsers[this.type, Parser]
    def errorsLocation[A](p: Parser[A]): Location = ???   //TODO
  }

}
