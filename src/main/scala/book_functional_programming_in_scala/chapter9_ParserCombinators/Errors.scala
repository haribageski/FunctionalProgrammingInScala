package book_functional_programming_in_scala.chapter9_ParserCombinators

object Errors {
  type ParserErrorMsg = String

  sealed trait CommittedStatusOfError
  object Committed extends CommittedStatusOfError
  object Uncommitted extends CommittedStatusOfError

  sealed class Location(val input: String)

  case class KnownLocation(location: Int, override val input: String) extends Location(input) {
    require(input.length > location)
    lazy val line = input.slice(0, location + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0,location + 1).lastIndexOf('\n') match {
      case -1 => location + 1
      case lineStart => location - lineStart }
  }
  case class UnknownLocation(override val input: String) extends Location(input)

  case class ParserError(msg: ParserErrorMsg)

  case class ParserErrors(errors: List[(Location, ParserErrorMsg)])


  object ParserError {
    trait ReportErrorSide
    case object ReportErrorOfLeftParser extends ReportErrorSide
    case object ReportErrorOfRightParser extends ReportErrorSide
    case object ReportErrorOfBothLeftAndRightParsers extends ReportErrorSide

    def apply(location: Location, expectedInput: String): ParserError = {
      location match {
        case KnownLocation(location, input) if(expectedInput.length <= location)  => apply(s"Error: Position $location is bigger than the input.")
        case KnownLocation(l, input) => apply(s"Error at position $l: Expected ${expectedInput.charAt(l)} but found ${location.input.charAt(l)}.")
        case UnknownLocation(input)  => apply(s"Error: Expected $expectedInput but found ${location.input}.")
      }
    }

    def errorsMsgs[A](p: Parser[A]): ParserErrors = ???   //TODO    implicit parsers: Parsers[this.type, Parser]
    def errorsLocation[A](p: Parser[A]): KnownLocation = ???   //TODO
  }

}
