package book_functional_programming_in_scala.chapter9_ParserCombinators

object Errors {
  type ParserErrorMsg = String

  case class ParseState(location: Int = 0, input: String, isParsed: Boolean = false) {    //if isParsed = true, then we don't want to return the element generated in the parsing, only the resulting location
    lazy val line = input.slice(0, location + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0,location + 1).lastIndexOf('\n') match {
      case -1 => location + 1
      case lineStart => location - lineStart }

    def advanceBy(n: Int): Option[ParseState] = {
      println("advanceBy" + n)
      if(location + n > input.length )   None
      else  Some(copy(location = location + n, input))
    }
  }

  case class ParserError(location: ParseState, expectedInput: String, isImportant: Boolean) {
    def errorMessage: ParserErrorMsg = {
//       if(expectedInput.length <= location.location)    s"Position $location is bigger than the input."
       s"Expected '$expectedInput' but found '${location.input.charAt(location.location)}'."
    }
  }

  object ParserError {
    def errorsMsgs[A](p: Parser[A]): ParserErrors = ???   //TODO    implicit parsers: Parsers[this.type, Parser]
    def errorsLocation[A](p: Parser[A]): ParseState = ???   //TODO
  }

  case class ParserErrors(errors: List[ParserError], isImportant: Boolean) {
    def msg = errors.groupBy(_.location.location).foldLeft("") {
      case (acc, (location, errorsAtOneLocation)) => acc + '\n' + {
        s"Error at position $location: ${errorsAtOneLocation.foldLeft("")(_ + "\n\t" + _.errorMessage)}"
      }
    }
    def push(loc: ParseState, expectedInput: String, isImportant: Boolean): ParserErrors =
      copy(errors = ParserError(loc, expectedInput, isImportant) :: errors, isImportant = isImportant || this.isImportant)
    def label[A](s: String): ParserErrors = this.copy(
      latestLoc.flatMap(state => latest.map(error => (state, error.isImportant)))
        .map{case ((state, isImportant)) => ParserError(state, s, isImportant)}
        .toList
    )    //Change the error message of the last message
    def latestLoc: Option[ParseState] = latest.map(_.location)
    def latest: Option[ParserError] = errors.lastOption     //Last message is the most detailed from inner scopes
  }

  object ParserErrors {
    def apply(): ParserErrors = apply(Nil, false)
  }
}
