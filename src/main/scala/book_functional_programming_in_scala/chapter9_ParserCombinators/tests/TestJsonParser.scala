package book_functional_programming_in_scala.chapter9_ParserCombinators.tests

import book_functional_programming_in_scala.chapter6_PurelyFunctionalState.SimpleRNG
import book_functional_programming_in_scala.chapter8_PropertyBasedTesting.Gen
import book_functional_programming_in_scala.chapter9_ParserCombinators.Errors.Location
import book_functional_programming_in_scala.chapter9_ParserCombinators.{JSON, JsonParser, MyParsers, Parser}
import book_functional_programming_in_scala.chapter9_ParserCombinators.MyParsers._


object TestJsonParser extends App{

//  val propForMap = MyParsers.Laws.mapLaw(stringLiteral)(Gen.string(15), Gen.genStringFn(Gen.int))
//  println(propForMap.run(5, 100, SimpleRNG(100)))
//
//
//  val line = stringLiteralWithoutQuotes ** (
//    whitespace.many1 skipLeftAndTakeRight char(':'))
//  println(line.run(Location(0,
//    """"a" :""".stripMargin)))
//
//  val propForString = MyParsers.Laws.stringInQuotesLaw
//  println(propForString.run(5, 1000, SimpleRNG(1002)))





//  val wrongJson = jsonParser.run(Location(0,
//    """wrong json""".stripMargin))
//  println("wrongJson:" + wrongJson)


//  Works:
//  val x = char('b').flatMap(a => many(char('a')))
//  println(x.run(Location(0, "baaab")))
//  val stringLiteral = stringLiteralWithoutQuotes
//  println(stringLiteral.run(Location(0, """"hari"""")))
//  println(char.until(char('d')).map2(char('d')) { case (l, a) => a :: l }.run(Location(0, "ad")))
//  println(char('s').skipLeftAndTakeRight(char('h')).skipLeftAndTakeRight('d').run(Location(0,"""shd""")))
//  println(char('s').skipLeftAndTakeRight(char.until(char('h'))).run(Location(0,"""saehd""")))
//  println(char('"').skipLeftAndTakeRight(char.until(char('"'))).run(Location(0, """"har"i""")))
//  println(stringLiteralWithoutQuotes.run(Location(0, "\"hari\"")))
//  def processline = whitespace.many skipLeftAndTakeRight stringLiteralWithoutQuotes ** (
//  whitespace.many skipLeftAndTakeRight char(':') skipLeftAndTakeRight whitespace.many1 skipLeftAndTakeRight char.until(char(',') | char('}')) takeLeftAndSkipRight (char(',') | char('}')))
//  println(processline.run(Location(0, """"key" : 1""")))
//  println((processline.many)
//    .run(Location(0,
//      """"key1" : 1,"key2" : 2,"key3" : 3}
//      """.stripMargin))
//  )
//  ----------------------------------------------

  lazy val jsonParser: Parser[JSON] = JsonParser.jsonParser(MyParsers)
  lazy val json1 = jsonParser.run(Location(0,
    """{"1" : 1,"2" : {"3" : true}}"""))

  println(json1)

}
