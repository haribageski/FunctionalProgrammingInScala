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
  def run[A](p: Parser[A])(input: String): Parser.Result[A] = p.run(Location(0, input))

  def run[A](p: Parser[A])(location: Location): Parser.Result[A] = p.run(location)

  def or[A, B >: A](s1: Parser[A], s2: => Parser[B]): Parser[B] =
    s1.copy(errorLocation => s1.run(errorLocation) match {
      case Failure(get, false) => s2.run(errorLocation)
      case r => r
    })

  def and[A, B >: A](s1: Parser[A], s2: => Parser[B]): Parser[B] =
    s1.copy(errorLocation => s1.run(errorLocation) match {
      case Success(a, charsConsumedA) =>
        s2.run(errorLocation) match {
          case Success(b, charsConsumedB) =>
            if (a == b && charsConsumedA == charsConsumedB) Success(b, charsConsumedB)
            else Failure(ParserErrors().push(Location(charsConsumedB + errorLocation.location, errorLocation.input), "input that gives same results for both parsers"), false)
          case f => f
        }
      case f => f
    })

  def attempt[A](p: Parser[A]): Parser[A] = p.copy(errorLocat => p.run(errorLocat).uncommit) //converts committed to uncommitted Failure (in case of Failure)

  def flatMap[A, B](f: Parser[A])(g: A => Parser[B]): Parser[B] = Parser {
    //apply input on Parser[A] and then apply input advanced by Parser[A] to Parser[B]
    s => f.run(s) match {
      case Success(a, n) => {
        val updatedLocation: Location = s.copy(s.location + n)
        g(a).run(updatedLocation) //we apply to Parser[B] the input advanced by n charachers, where n is the num of characters that Parser[A] advanced
              .addCommit(n != 0) //after running Parser[B], in case of success we change the committed status to true if the input is advanced by at least one char
              .advanceSuccess(n) //in case Parser[B] Succeed, we update the parser location on the input
      }
      case e@Failure(_, _) => e
    }
  }

  implicit def string(s: String): Parser[String] = {
    def consume(locationInInput: Location, startingLocation: Location)(strToMatch: String): Parser.Result[String] = {
      val location = locationInInput.location
      val input = locationInInput.input

      if(strToMatch.isEmpty) Success(s, s.length)
      else if (location >= input.length || input.charAt(location) != strToMatch.head)
        if (startingLocation != locationInInput) Failure(ParserErrors().push(Location(location, input), strToMatch), false)
        else Failure(ParserErrors().push(Location(location, input), strToMatch), true)
      else {
        consume(locationInInput.copy(location + 1), startingLocation)(strToMatch.tail)
      }
    }
    Parser {
      location =>
        consume(location, location)(s)
    }
    //    scope(s"Input doesn't start with $s at the specified starting index.")(parser)
  }

  def succeed[A](elem: A): Parser[A] = Parser(location => Success(elem, 0))

  def failed[A](e: ParserErrorMsg): Parser[A] = Parser(
    location => Failure(ParserErrors(List(ParserError(Location(location.location, "nothing"), e))), false)
  ) //primitive: The resulting Parser always returns Error(e) when run.

  implicit def regex(r: Regex): Parser[String] = {
    Parser(
      location => {
        r.findFirstIn(location.input.substring(location.location)) match {
          case Some(str) =>
            Success(str, str.length)
          case None =>
            Failure(ParserErrors(List(ParserError(location, r.pattern.pattern))), false)
        }
      }
    )
  } //primitive: Recognizes a regular expression s

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
  def furthest[A](p: Parser[A]): Parser[A] = ???
  //primitive: In case we apply || to this parser and another, in case both fail the errors from the one that reached furthest character will be returned.
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

  def many[A](p: Parser[A]): Parser[List[A]] = {
    def iterateRecursively(startingLocation: Location, location: Location, acc: List[A]): Success[List[A]] = {
      if(location.input.length < location.location)   Success(acc, location.location - startingLocation.location)
      else  {
        p.run(location) match {
          case Success(get, charsConsumed) =>
            val updatedLocation: Location = location.copy(location.location + charsConsumed)
            iterateRecursively(startingLocation, updatedLocation, get :: acc)
          case Failure(get, isCommitted) => Success(acc.reverse, location.location - startingLocation.location) //for better performance, we have been prepending elements to the acc, hence, we need to reverse it at the end
        }
      }
    }
    Parser(location =>
      if(location.input.length < location.location)   Success(List.empty[A], 0)
      else    iterateRecursively(location, location, Nil))
  }

  def until[A, B](p: Parser[A], until: Parser[B]): Parser[List[A]] = {
    def iterateRecursively(startingLocation: Location, currentLocation: Location, acc: List[A]): Success[List[A]] = {
      if(currentLocation.input.length < currentLocation.location)   Success(acc, currentLocation.location - startingLocation.location)
      else {
        until.run(currentLocation) match {
          case Success(get, charsConsumed) =>
            Success(acc.reverse, currentLocation.location - startingLocation.location)
          case _ =>
            p.run(currentLocation) match {
              case Success(get, charsConsumed) =>
                val updatedLocation: Location = currentLocation.copy(currentLocation.location + charsConsumed)
                iterateRecursively(startingLocation, updatedLocation, get :: acc)
              case Failure(get, isCommitted) => Success(acc.reverse, currentLocation.location - startingLocation.location) //done, for better performance, we have been prepending elements to the acc, hence, we need to reverse it at the end
            }
        }
      }
    }
    Parser(
      location =>
        if(location.input.length < location.location)   Success(List.empty[A], 0)
        else  iterateRecursively(location, location, Nil)
    )
  }
}
