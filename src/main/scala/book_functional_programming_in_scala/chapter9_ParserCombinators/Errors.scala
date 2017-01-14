package book_functional_programming_in_scala.chapter9_ParserCombinators

object Errors {
  type ParserErrorMsg = String

  case class Location(location: Int, input: String) {
    lazy val line = input.slice(0, location + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0,location + 1).lastIndexOf('\n') match {
      case -1 => location + 1
      case lineStart => location - lineStart }

    def advanceBy(n: Int): Option[Location] = {
      println("advanceBy" + n)
      if(location + n > input.length )   None
      else  Some(copy(location = location + n, input))
    }
  }

  case class ParserError(location: Location, expectedInput: String) {
    def errorMessage: ParserErrorMsg = {
       if(expectedInput.length <= location.location)    s"Error: Position $location is bigger than the input."
       else  s"Error at position ${location.location}: Expected ${expectedInput.charAt(location.location)} but found ${location.input.charAt(location.location)}."
    }
  }

  object ParserError {
    def errorsMsgs[A](p: Parser[A]): ParserErrors = ???   //TODO    implicit parsers: Parsers[this.type, Parser]
    def errorsLocation[A](p: Parser[A]): Location = ???   //TODO
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
