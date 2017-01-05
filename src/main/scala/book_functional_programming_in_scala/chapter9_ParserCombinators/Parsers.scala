package book_functional_programming_in_scala.chapter9_ParserCombinators

import book_functional_programming_in_scala.chapter8_PropertyBasedTesting.{Gen, Prop}
import book_functional_programming_in_scala.chapter8_PropertyBasedTesting.Prop._

trait Parsers[ParserError, Parser[+ _]]{ self =>    //[+ _] is used when the outer type is a type constructor itself

  def run[A](p: Parser[A])(input: String): Either[ParserError, A]   //representation
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]  //primitive
  def map[A, B](p: Parser[A])(f: A => B): Parser[B]   //primitive
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)]   //primitive, lazy second argument is necessary, otherwise map2() will never terminate
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = product(p, p2).map(f.tupled) //lazy second argument is necessary, otherwise many() will never terminate
  def succeed[A](unit: A): Parser[A] = string("").map(_ => unit)

  implicit def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  implicit def string(s: String): Parser[String]    //primitive
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  def slice[A](p: Parser[A]): Parser[String]    //primitive
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
  def occurrences(c: Char): Parser[Int] = char(c).slice.map(_.length)
  val numA = occurrences('a')
  def occurrencesAtLeastOne(c: Char): Parser[Int]
  def manyWithMany1[A, B](a: Parser[A], b: Parser[B]): Parser[(Int, Int)] =
    char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)



  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def map2[B, C](p2: Parser[B])(f: ((A, B)) => C): Parser[C] = self.map2(p, p2)(f)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def slice[A]: Parser[String] = self.slice(p)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }


  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A, B](p: Parser[A])(gen: Gen[String], genFunc: Gen[String => B]): Prop =
      forAll(gen)(s => run(p.map(a => a))(s) == run(p)) //&&
    //forAll(gen.flatMap(s => genFunc.map(func => (s, func))))(strAndFunc =>
      //run(p.map(a => strAndFunc._2(a.toString)))(strAndFunc._1)) == run()

    def unit[A](a: A)(gen: Gen[String]): Prop =
      forAll(gen)(s => run(succeed(a))(s) == Right(a))

    def product[A, B](p1: Parser[A], p2: Parser[B])(gen: Gen[String]): Prop =
      forAll(gen)(s => {
        val resOfParser1E: Either[ParserError, A] = run(p1)(s)
        val restOfStringOfParsing1E: Either[ParserError, String] = run(p1.slice.map(slice => s.substring(slice.length)))(s)

        val resOfParser2E: Either[ParserError, B] = restOfStringOfParsing1E.fold(e => Left(e), str2 => {
          run(p2)(str2)
        })

        resOfParser1E.fold(e => Left(e), a => resOfParser2E.fold(Left(_), Right(a, _))) == run(p1 ** p2)(s)
      })
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
