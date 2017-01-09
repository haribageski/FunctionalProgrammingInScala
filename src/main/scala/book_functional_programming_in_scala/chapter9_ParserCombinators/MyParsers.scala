package book_functional_programming_in_scala.chapter9_ParserCombinators

import book_functional_programming_in_scala.chapter8_PropertyBasedTesting.Prop._
import book_functional_programming_in_scala.chapter9_ParserCombinators.Errors._
import book_functional_programming_in_scala.chapter9_ParserCombinators.Parser.InspectedInput

import scala.util.Left
import scala.util.matching.Regex

/**
  * Library for parsing.
  * By default a Parser is in committed state, meaning if we apply or to it (as a left branch), and if both branches fail,
  * we take the error from the left branch. If in uncommitted state, we take the error from the right branch.
  */
object MyParsers extends Parsers[Parser] {
  override def run[A](p: Parser[A])(input: String): Either[ParserErrors, A] = p.run(input).right.map(_._1)

  override def or[A, B >: A](s1: Parser[A], s2: => Parser[B]): Parser[B] = Parser(
    input => {
      s1.run(input) match {
        case Left(errors) =>
          s2.run(input)
            .left.map{ errors2 =>
            (s1.committedStatusOfError, s2.committedStatusOfError) match {
              case (Committed, Committed)   => ParserErrors(errors.errors ::: errors2.errors)
              case (Committed, Uncommitted) => errors
              case (Uncommitted, _)         => errors2
            }}
        case r: Right[ParserErrors, B] => r
      }
    }, (s1.committedStatusOfError, s2.committedStatusOfError) match {
      case (Uncommitted, Uncommitted) =>  Uncommitted
      case _                          =>  Committed
    }
  )

  def attempt[A](p: Parser[A]): Parser[A] = p.copy(committedStatusOfError = Uncommitted)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = {
    val outsideErrorOrInsideResult: (String) => Parser[B] =
      (input: String) => {
        p.run(input) match {
          case Left(errors) => p.copy(input => Left(errors))
          case Right((a, _)) => f(a)
        }
      }
    flatten(
      Parser(input =>
        p.run(input).right.map(resultWithInput => (outsideErrorOrInsideResult(input), resultWithInput._2)),
        p.committedStatusOfError
      )
    )
  }   //primitive: Runs a parser, and then uses its result to select a second parser to run in sequence



  implicit def string(s: String): Parser[String] = {
    @scala.annotation.tailrec
    def matchInput(input: String, positionToMatch: Int): Either[ParserErrors, (String, InspectedInput)] = positionToMatch match {
      case i if i == s.size => Right((s, s))
      case i if i >= input.length || (input.charAt(i) != s.charAt(i)) => Left(ParserErrors(List((KnownLocation(i, input), ParserError(KnownLocation(i, input), s.charAt(i).toString).msg))))
      case i => matchInput(input, positionToMatch + 1)
    }
    Parser(
      input => matchInput(input, 0)
    )
  }

  def failed[A](e: ParserErrorMsg)(p: Parser[A]): Parser[A] = {
    p.copy(input => Left(ParserErrors(List((UnknownLocation(input), e)))))
  }//primitive: The resulting Parser always returns Error(e) when run.

  implicit def regex(r: Regex): Parser[String] = Parser(
    input => {
      r.findFirstIn(input) match {
        case Some(str) => Right((str, str))
        case None =>      Left(ParserErrors(List((UnknownLocation(input), ParserError(UnknownLocation(input), r.pattern.pattern).msg))))
      }
    }
  ) //primitive: Recognizes a regular expression s

  def scope[A](errorsToAdd: ParserErrors)(p: Parser[A]): Parser[A] = p.copy(input => p.run(input).left.map(errors => ParserErrors(errorsToAdd.errors ::: errors.errors)))

  def label[A](e: ParserErrorMsg)(p: Parser[A]): Parser[A] =
    p.copy(input => p.run(input).left.map(_ => ParserErrors(List((UnknownLocation(input), e)))))

  override def slice[A](p: Parser[A]): Parser[String] = p.copy(input => p.run(input).right.map {
    case (_, i: InspectedInput) => (i, i)
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
