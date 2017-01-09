package book_functional_programming_in_scala.chapter9_ParserCombinators

import book_functional_programming_in_scala.chapter6_PurelyFunctionalState.SimpleRNG
import book_functional_programming_in_scala.chapter8_PropertyBasedTesting.Prop._
import book_functional_programming_in_scala.chapter8_PropertyBasedTesting.{Gen, Prop, SGen}
import book_functional_programming_in_scala.chapter9_ParserCombinators.Errors._

import scala.util.matching.Regex

/**
  * Library for parsing.
  * By default a Parser is in committed state, meaning if we apply or to it (as a left branch), and if both branches fail,
  * we take the error from the left branch. If in uncommitted state, we take the error from the right branch.
  */
trait Parsers[Parser[+ _]] { self =>
  //[+ _] is used when the outer type is a type constructor itself

  def run[A](p: Parser[A])(input: String): Either[ParserErrors, A]
  //representation
  def or[A, B >: A](s1: Parser[A], s2: => Parser[B]): Parser[B]
  //primitive
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(a => succeed(f(a)))
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] = p1.flatMap(a => p2.map((a, _)))
  //lazy second argument is necessary, otherwise map2() will never terminate
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = p.flatMap(a => p2.map(f(a, _)))
  //lazy second argument is necessary, otherwise many() will never terminate
  def succeed[A](elem: A): Parser[A]
  def failed[A](e: ParserErrorMsg)(p: Parser[A]): Parser[A]   //primitive: The resulting Parser always returns Error(e) when run.
  def label[A](e: ParserErrorMsg)(p: Parser[A]): Parser[A]    //primitive: In the event of failure, replaces the assigned message with e
  def scope[A](errorMsg: String)(p: Parser[A]): Parser[A]    //It adds the error on top of the existing errors.
  def attempt[A](p: Parser[A]): Parser[A]   //Change the state to un-committed.

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def flatten[A](p: Parser[Parser[A]]): Parser[A]

  def seq[A](l: List[Parser[A]]): Parser[List[A]] = l.foldLeft(succeed(List.empty[A]))((acc, parserA) => map2(parserA, acc)(_ :: _))
    .map(_.reverse)
  implicit def regex(r: Regex): Parser[String]
  //primitive
  def thatManyChars(c: Char) = "[0-9]*".r //we extract the number of character from the beginning of the string we are parsing
    .flatMap(strMatched => listOfN(strMatched.toInt, char(c)))
  def skipFirstAndTakeSecond[A, B](s1: => Parser[A], s2: Parser[B]): Parser[B] = map2(s1, s2)((_, b) => b)
  def takeFirstAndSkipSecond[A, B](s1: Parser[A], s2: => Parser[B]): Parser[A] = map2(s1, s2)((a, _) => a)

  implicit def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  implicit def string(s: String): Parser[String]
  //primitive
  def char: Parser[Char] = regex(""".""".r).map(_.charAt(0))
  //TODO: special symbols to be added
  def digit: Parser[Char] = regex("""\\d""".r).map(_.charAt(0))
  def whitespace: Parser[String] = regex("""\\s""".r)
  def stringLiteralWithoutQuotes: Parser[String] = char('"').skipAndTakeNext(stringLiteral).takeAndSkipNext(char('"'))
  def stringLiteral: Parser[String] = char.many.map(_.toString)
  def int = digit.many1.map(_.toString.toInt)
  def double: Parser[Double] = regex("""[0-9]+.[0-9]+e?[0-9]*""".r).map(_.toDouble)
  def bool: Parser[Boolean] = regex("""true|false""".r).map(_.toBoolean)

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  def slice[A](p: Parser[A]): Parser[String]    //primitive: Returns the portion of input inspected by p if successful.
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case 0 => succeed(Nil)
    case _ => map2(p, listOfN(n - 1, p))(_ :: _)
  }
  def many[A](p: Parser[A]): Parser[List[A]] = {
    //Slower but no stack overflow:
    @scala.annotation.tailrec
    def iterateRecursively(acc: Parser[List[A]]): Parser[List[A]] = {
      val a = p.map(List(_)) or succeed(Nil)
      val nextAcc: Parser[List[A]] = map2(a, acc)(_ ::: _)
      iterateRecursively(nextAcc)
    }
    iterateRecursively(succeed(Nil))

    //Faster but danger of stack overflow:
    //map2(p, many(p))(_ :: _) or succeed(Nil)
  }
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
  //at least one
  def occurrences(c: Char): Parser[Int] = char(c).slice.map(_.length)
  val numA = occurrences('a')
  def occurrencesAtLeastOne(c: Char): Parser[Int]
  def manyaWithMany1b[A, B](a: Parser[A], b: Parser[B]): Parser[(Int, Int)] =
    char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def slice[A]: Parser[String] = self.slice(p)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def skipAndTakeNext[B](s2: Parser[B]): Parser[B] = skipFirstAndTakeSecond(p, s2)
    def takeAndSkipNext[B](s2: => Parser[B]): Parser[A] = takeFirstAndSkipSecond(p, s2)
    def failed(e: ParserErrorMsg): Parser[A] = self.failed(e)(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A, B](p: Parser[A])(gen: Gen[String], genFunc: Gen[String => B]): Prop =
      forAll(gen)((s: String) => run(p.map(a => a))(s) == run(p)(s)) //&&
    //forAll(gen.flatMap(s => genFunc.map(func => (s, func))))(strAndFunc =>
    //run(p.map(a => strAndFunc._2(a.toString)))(strAndFunc._1)) == run()

    def unit[A](a: A)(gen: Gen[String]): Prop =
      forAll(gen)(s => run(succeed(a))(s) == Right(a))

    //The product of two parsers must result with the error of the left parser if it results with an error.
    def product[A, B](p1: Parser[A], p2: Parser[B])(gen: Gen[String]): Prop =
      forAll(gen)(s => {
        val resOfParser1E: Either[ParserErrors, A] = run(p1)(s)
        val restOfStringOfParsing1E: Either[ParserErrors, String] = run(p1.slice.map(slice => s.substring(slice.length)))(s)

        val resOfParser2E: Either[ParserErrors, B] = restOfStringOfParsing1E.fold(e => Left(e), str2 => {
          run(p2)(str2)
        })

        resOfParser1E.fold(e => Left(e), a => resOfParser2E.fold(Left(_), Right(a, _))) == run(p1 ** p2)(s)
      })

    //When parsing a string with some letter that is not as we expect, the parser must result with a descriptive message in a Left.
    def string(genStr: Gen[String]) =
      forAll(genStr)(s => {
        val parser: Parser[String] = self.string(s)

        Gen.choose(0, s.length - 1).map(location => {
          val strWithChangedLetter: String = s.substring(0, location) ++ (s.charAt(location) match {
            case c if c.isUpper => c.toString.toLowerCase
            case c => c.toString.toUpperCase
          }) ++ s.substring(location + 1, s.length)

          run(parser)(strWithChangedLetter) == Left(ParserErrors(List(ParserError(KnownLocation(location, strWithChangedLetter), s))))
        }).sample.run(SimpleRNG(0))._1
      })

    def labelLaw[A](p: Parser[A], inputsGen: SGen[String], errorMsgGen: SGen[ParserErrorMsg]): Prop =
      forAll(inputsGen ** errorMsgGen) { case (input: String, errorMsg: ParserErrorMsg) =>
        run(failed(errorMsg)(p))(input) match {
          case Left(e: Errors.ParserErrors) => e.msg == errorMsg
          case _ => false
        }
      }

    /** Laws:
      * run(char(c))(c.toString) == Right(c)
      * run(string(s))(s.toString) == Right(s)
      *
      * Or: it tries p1 on the input, and then tries p2 only if p1 fails
      * run(or(string("aba"), string("ab"))("abab") == Right("aba")
      * run(or(string("aba"), string("ab"))("ab") == Right("ab")
      * run(or(string("abra"), string("kadabra"))("abra") == Right("abra")
      * run(or(string("abra"), string("kadabra"))("kadabra") == Right("kadabra")
      *
      * run(listOfN(3, "ab" | "cad"))("ababcad") == Rignt("ababcad")
      * run(listOfN(3, "ab" | "cad"))("cadabab") == Rignt("cadabab")
      * run(listOfN(3, "ab" | "cad"))("ababab") == Rignt("ababab")
      * run(char("a") | Parser.unit)("ababab") == run(char("a"))("ababab")
      *
      * run(occurrences("a"))("aa") == Right(2)
      * run(occurrences("a"))("") == Right(0)
      * run(occurrences("a"))("ba") == Right(0)
      *
      * run(occurrencesAtLeastOne("a"))("ba") == Left("Expected one or more 'a'")
      * run(occurrencesAtLeastOne("a"))("aa") == Right("2")
      *
      * run(occurencesAndOccurrencesAtLeastOne("a", "b"))("bbb") == Right(0, 3)
      * run(occurencesAndOccurrencesAtLeastOne("a", "b"))("aaaab") == Right(3, 1)
      * run(occurencesAndOccurrencesAtLeastOne("a", "b"))("") == Left("Expected one or more 'b'")
      * run(occurencesAndOccurrencesAtLeastOne("a", "b"))("a") == Left("Expected one or more 'b'")
      *
      * Not necessarely: run(occurrencesAtLeastOne("a)" | occurrencesAtLeastOne("b"))("") == run(occurrencesAtLeastOne("a)" | occurrencesAtLeastOne("b"))("")
      * Associativity must hold.
      * Combiner(|) + Unit + Associativity => Monoid
      * + Map => Functor
      *
      *
      */
  }
}
