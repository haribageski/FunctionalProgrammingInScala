package book_functional_programming_in_scala.chapter9_ParserCombinators

import book_functional_programming_in_scala.chapter8_PropertyBasedTesting.Prop._
import book_functional_programming_in_scala.chapter9_ParserCombinators.Errors._
import book_functional_programming_in_scala.chapter9_ParserCombinators.Parser.{Failure, Success}

import scala.util.matching.Regex

/**
  * Library for parsing.
  * By default a Parser is in committed state, meaning if we apply or to it (as a left branch), and if both branches fail,
  * we take the error from the left branch. If in uncommitted state, we take the error from the right branch.
  */
object MyParsers extends Parsers[Parser] {
  override def run[A](p: Parser[A])(input: String): Either[ParserErrors, A] = ??? //p.run(input).right.map(_._1)

  override def or[A, B >: A](s1: Parser[A], s2: => Parser[B]): Parser[B] =
    s1.copy(errorLocation => s1.run(errorLocation) match {
      case Failure(get, false)    =>    s2.run(errorLocation)
      case r                      => r
    })

  def attempt[A](p: Parser[A]): Parser[A] = p.copy(errorLocat => p.run(errorLocat).uncommit)    //converts committed to uncommitted Failure (in case of Failure)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???
//  {
//    val outsideErrorOrInsideResult: (String) => Parser[B] =
//      (input: String) => {
//        p.run(input) match {
//          case Left(errors) => p.copy(input => Left(errors))
//          case Right((a, _)) => f(a)
//        }
//      }
//    flatten(
//      Parser(input =>
//        p.run(input).right.map(resultWithInput => (outsideErrorOrInsideResult(input), resultWithInput._2)),
//        p.committedStatusOfError
//      )
//    )
//  }   //primitive: Runs a parser, and then uses its result to select a second parser to run in sequence



  implicit def string(s: String): Parser[String] = {
    def consume(locationInInput: Location)(index: Int, strToMatch: String): Parser.Result[String] = locationInInput match {
      case KnownLocation(location, input) => {
        if (strToMatch.isEmpty)    Success(s, s.length)
        else if(index + location >= input.length || input.charAt(index + location) != strToMatch.head)
          if(index == 0)  Failure(ParserErrors().push(KnownLocation(index + location, input), s), false)
          else            Failure(ParserErrors().push(KnownLocation(index + location, input), s), true)
        else   consume(locationInInput)(index + 1, strToMatch.tail)
      }
      case UnknownLocation(input) => Failure(ParserErrors().push(UnknownLocation(input), s), false)
    }
    val parser = Parser {
      location => consume(location)(0, location.input)
    }
    scope(s"Input doesn't start with $s at the specified starting index.")(parser)
  }

  def succeed[A](elem: A): Parser[A] = Parser(location => Success(elem, 0))

  def failed[A](e: ParserErrorMsg)(p: Parser[A]): Parser[A] = {
    p.copy(location => Failure(ParserErrors(List(ParserError(UnknownLocation(location.input), e))), false))
  }//primitive: The resulting Parser always returns Error(e) when run.

  implicit def regex(r: Regex): Parser[String] = Parser(
    location => {
      r.findFirstIn(location.input) match {
        case Some(str) => Success(str, str.length)
        case None =>      Failure(ParserErrors(List(ParserError(location, r.pattern.pattern))), false)
      }
    }
  ) //primitive: Recognizes a regular expression s

  def scope[A](errorMsg: ParserErrorMsg)(p: Parser[A]): Parser[A] = Parser {
    location => p.run(location).mapError(_.push(location, errorMsg))
  }

  def label[A](errorMsg: ParserErrorMsg)(p: Parser[A]): Parser[A] = Parser {
    location => p.run(location).mapError(_ => ParserErrors().push(location, errorMsg))
  }

  override def slice[A](p: Parser[A]): Parser[String] = p.copy(location => p.run(location) match {
    case Success(get, charsConsumed) => Success(location.input.take(charsConsumed), charsConsumed)
    case Failure(get, isCommitted) => Failure(get, isCommitted)
  })

  override def occurrencesAtLeastOne(c: Char): Parser[SuccessCount] = ???
  def furthest[A](p: Parser[A]): Parser[A] = ???    //primitive: In case we apply || to this parser and another, in case both fail the errors from the one that reached furthest character will be returned.
  //  {
  //    outside after inside, errors_inside ::: errors_outside,
  //    Parser(
  //      input => {
  //        val outerErrorsOrInsideParser: Either[ParserErrors, Parser[A]] = p.evalAndRun(input)
  //        val insideErrorsOrResult: Either[ParserErrors, A] = outerErrorsOrInsideParser.fold[Either[ParserErrors, A]](Left(_), _.evalAndRun(input))
  //        val combinedErrorsOrResult: Either[ParserErrors, A] = outerErrorsOrInsideParser.left.flatMap[A, ParserErrors](outerErrors =>
  //          insideErrorsOrResult.left.map[ParserErrors](insideErrors => ParserErrors(insideErrors.errors ::: outerErrors.errors))
  //        )
  //        combinedErrorsOrResult
  //      }, p.committedStatusOfError, p.errors)
  //
  //  }     //primitive
  def flatten[A](p: Parser[Parser[A]]): Parser[A] = ???

}
