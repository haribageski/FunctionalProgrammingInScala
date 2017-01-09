package book_functional_programming_in_scala.chapter9_ParserCombinators

object Errors {
  type ParserErrorMsg = String

  sealed class Location(val input: String)

  case class KnownLocation(location: Int, override val input: String) extends Location(input) {
    require(input.length > location)
    lazy val line = input.slice(0, location + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0,location + 1).lastIndexOf('\n') match {
      case -1 => location + 1
      case lineStart => location - lineStart }
  }
  case class UnknownLocation(override val input: String) extends Location(input)


  case class ParserError(location: Location, expectedInput: String) {

    def errorMessage: ParserErrorMsg = location match {
      case KnownLocation(location, input) if(expectedInput.length <= location)  =>
        s"Error: Position $location is bigger than the input."
      case KnownLocation(l, input)    => s"Error at position $l: Expected ${expectedInput.charAt(l)} but found ${location.input.charAt(l)}."
      case UnknownLocation(input)     => s"Error: Expected $expectedInput but found ${location.input}."
    }
  }

  object ParserError {
    def errorsMsgs[A](p: Parser[A]): ParserErrors = ???   //TODO    implicit parsers: Parsers[this.type, Parser]
    def errorsLocation[A](p: Parser[A]): KnownLocation = ???   //TODO
  }

  case class ParserErrors(errors: List[ParserError]) {
    def msg = errors.foldLeft("")(_ + '\n' + _.errorMessage)
    def push(loc: Location, expectedInput: String): ParserErrors = copy(errors = ParserError(loc, expectedInput) :: errors)
    def label[A](s: String): ParserErrors = ParserErrors(latestLoc.map(ParserError(_,s)).toList)    //Change the error message of the last message
    def latestLoc: Option[Location] = latest.map(_.location)
    def latest: Option[ParserError] = errors.lastOption     //Last message is the most detailed from inner scopes
  }

  object ParserErrors {
    def apply(): ParserErrors = apply(Nil)
  }
}
