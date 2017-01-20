package book_functional_programming_in_scala.chapter9_ParserCombinators
import book_functional_programming_in_scala.chapter9_ParserCombinators.JSON.{JArray, JBool, JNull, JNumber, JObject, JString}


object JsonParser {
  def jsonParser[P, Parser[+P]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    def jNullParser: Parser[JNull.type] = {
      string("null").map (_ => JNull)
    }

    def jNumberParser: Parser[JNumber] = (attempt(double) | int.map(_.toDouble)).map(JNumber(_)).scope("Failed to parse a JNumber.")

    def jStringParser: Parser[JString] = stringLiteralWithoutQuotes.map(JString(_)).scope("Failed to parse a JString.")

    def jBoolParser: Parser[JBool] = bool.map(JBool(_)).scope("Failed to parse a JBool.")

    def jLiteralParser = attempt(jBoolParser)| attempt(jNullParser) | attempt(jNumberParser) | attempt(jStringParser)


    def jArrayParser: Parser[JArray] = {
      (char('[') skipLeftAndTakeRight ((attempt(jObjectParser) | attempt(jArrayParser) | attempt(jLiteralParser))
        takeLeftAndSkipRight attempt(char(',')) | char(']')).many1
        ).map(list => JArray(list.toIndexedSeq))
        .scope("Failed to parse a JArray.")
    }

    def processLine: Parser[(String, JSON)] = {
      whitespace.many skipLeftAndTakeRight stringLiteralWithoutQuotes ** (
        whitespace.many skipLeftAndTakeRight char(':') skipLeftAndTakeRight whitespace.many skipLeftAndTakeRight
          (attempt(jObjectParser) | attempt(jArrayParser) | attempt(jLiteralParser)) takeLeftAndSkipRight (attempt(char(',')) | char('}')))
    }

    def jObjectParser: Parser[JSON] = {
      //      val keyValue: Parser[(String, JSON)] = ???
      //      val jObject: Parser[JObject] = ???
      char('{').flatMap { _ =>
        attempt(processLine.many1.map(l => Map(l: _*)).map(JObject(_))) | attempt(jObjectParser) |
          attempt(jArrayParser).takeLeftAndSkipRight(char('}')) | attempt(jLiteralParser).takeLeftAndSkipRight(char('}'))
      }
    }

    jObjectParser
  }
}
