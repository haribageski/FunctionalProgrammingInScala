package book_functional_programming_in_scala.chapter9_ParserCombinators

case class Message(expected: String, found: String)
case class ParserError(location: Int, msgs: Message)
case class ParserErrors(errors: Set[ParserError])

object ParserError {
  trait ReportErrorSide
  case object ReportErrorOfLeftParser extends ReportErrorSide
  case object ReportErrorOfRightParser extends ReportErrorSide
  case object ReportErrorOfBothLeftAndRightParsers extends ReportErrorSide
}
