package book_functional_programming_in_scala.chapter9_ParserCombinators
import book_functional_programming_in_scala.chapter9_ParserCombinators.JSON.{JArray, JBool, JNull, JNumber, JObject, JString}

class JsonParser extends Parser[JSON]


object JsonParser{
  def jsonParser[Err,Parser[+_]](P: Parsers[Err,Parser]): Parser[JSON] = {
    import P._

    val jNullParser: Parser[JNull.type] = string("null").map(_ => JNull)

    def jNumberParser: Parser[JNumber] = (double | int.map(_.toDouble)).map(JNumber(_))
    def jStringParser: Parser[JString] = stringLiteralWithoutQuotes.map(JString(_))
    def jBoolParser: Parser[JBool] = bool.map(JBool(_))
    def jLiteralParser = jBoolParser | jNullParser | jNumberParser | jStringParser

    def jArrayParser: Parser[JArray] = {
      regex("""*\[s*""".r) skipAndTakeNext
        (jObjectParser | jArrayParser | jNullParser | jLiteralParser)
        .takeAndSkipNext(regex("""s*\]|,s*""".r))
        .many
        .map(list => JArray(list.toArray))
    }

    def jObjectParser: Parser[JObject] = {
      val keyValue: Parser[(String, JSON)] =
        whitespace.many skipAndTakeNext stringLiteralWithoutQuotes ** (
          whitespace.many skipAndTakeNext char(':') skipAndTakeNext whitespace.many skipAndTakeNext
            (jObjectParser | jArrayParser | jNullParser | jLiteralParser) takeAndSkipNext regex("""s*,|$""".r)
          )
      val jObject: Parser[JObject] = keyValue.many
        .map(l => Map(l :_*))
        .map(JObject(_))

      char('{') skipAndTakeNext jObject takeAndSkipNext char('}')
    }

    jObjectParser
  }
}
