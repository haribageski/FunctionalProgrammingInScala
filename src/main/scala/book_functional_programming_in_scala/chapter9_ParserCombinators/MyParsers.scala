package book_functional_programming_in_scala.chapter9_ParserCombinators

import book_functional_programming_in_scala.chapter8_PropertyBasedTesting.Prop._
import book_functional_programming_in_scala.chapter9_ParserCombinators.Errors._
import book_functional_programming_in_scala.chapter9_ParserCombinators.Parser.{Failure, InspectedInput, Success}

import scala.util.Left
import scala.util.matching.Regex

/**
  * Library for parsing.
  * By default a Parser is in committed state, meaning if we apply or to it (as a left branch), and if both branches fail,
  * we take the error from the left branch. If in uncommitted state, we take the error from the right branch.
  */
object MyParsers extends Parsers[Parser] {
  override def run[A](p: Parser[A])(input: String): Either[ParserErrors, A] = ??? //p.run(input).right.map(_._1)

  override def or[A, B >: A](s1: Parser[A], s2: => Parser[B]): Parser[B] = ???
//  Parser(
//    input => {
//      s1.run(input) match {
//        case Left(errors) =>
//          s2.run(input)
//            .left.map{ errors2 =>
//            (s1.committedStatusOfError, s2.committedStatusOfError) match {
//              case (Committed, Committed)   => ParserErrors(errors.errors ::: errors2.errors)
//              case (Committed, Uncommitted) => errors
//              case (Uncommitted, _)         => errors2
//            }}
//        case r: Right[ParserErrors, B] => r
//      }
//    }, (s1.committedStatusOfError, s2.committedStatusOfError) match {
//      case (Uncommitted, Uncommitted) =>  Uncommitted
//      case _                          =>  Committed
//    }
//  )

  def attempt[A](p: Parser[A]): Parser[A] = p.copy(committedStatusOfError = Uncommitted)

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
    val parser = Parser {
      case KnownLocation(location, input) =>
        if (input.startsWith(s, location)) Success(s, s.length)
        else Failure(ParserErrors().push(KnownLocation(location, input), ""))
      case UnknownLocation(input) => Failure(ParserErrors(List(ParserError(UnknownLocation(input), s))))
    }
    scope(s"Input doesn't start with $s at the specified starting index.")(parser)
  }

  def succeed[A](elem: A): Parser[A] = Parser(location => Success(elem, 0))

  def failed[A](e: ParserErrorMsg)(p: Parser[A]): Parser[A] = {
    p.copy(location => Failure(ParserErrors(List(ParserError(UnknownLocation(location.input), e)))))
  }//primitive: The resulting Parser always returns Error(e) when run.

  implicit def regex(r: Regex): Parser[String] = Parser(
    location => {
      r.findFirstIn(location.input) match {
        case Some(str) => Success(str, str.length)
        case None =>      Failure(ParserErrors(List(ParserError(location, r.pattern.pattern))))
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
    case Failure(get) => Failure(get)
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
