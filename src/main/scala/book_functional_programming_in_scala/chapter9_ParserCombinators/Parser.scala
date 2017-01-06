package book_functional_programming_in_scala.chapter9_ParserCombinators

case class Parser[A]()//(run: String => Either[ParserError, A])

object Parser {
//  def unit = Parser(_ => Left(ParserError(Set.empty)))
}
