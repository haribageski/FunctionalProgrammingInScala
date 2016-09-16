package book_advanced_scala.chapter6_applicatives

import scala.collection.immutable.Iterable
import scalaz.{Applicative, Failure, Success, Validation}
import scalaz.Scalaz._

object Validate {
  def main(args: Array[String]) {
    processWebForm(Map(("fea", " f"), ("", ""), ("", "0"), ("hari", "24"))).foreach(println)
  }

  import scalaz.syntax.validation._
  scalaz.syntax.std.string

  case class User(name: String, age: Int)

  trait Error

  case class NoValueProvided(msg: String) extends Error

  case class CannotConvertStringToInt(msg: String) extends Error

  case class NegativeInt(msg: String) extends Error

  type ErrorValidation[A] = Validation[List[Error], A]

  def processWebForm(nameAge: Map[String, String]): Iterable[ErrorValidation[User]] = {

    def checkIfEmpty(name: String): ErrorValidation[String] =
      name.isEmpty match {
        case true => Failure(List(NoValueProvided("Name cannot be empty")))
        case false => Success(name)
      }

    def convertYears(years: ErrorValidation[String]): ErrorValidation[Int] = years.flatMap(age =>
      age.parseInt
        .bimap(e => List(CannotConvertStringToInt("Excetion encounterred during conversion of String to Int")), x => x)
      )

    def parseYear(years: ErrorValidation[Int]): ErrorValidation[Int] =
      years.flatMap(age => {
        if (age > 0) years
        else Failure(List(NegativeInt("Years cannot be smaller 0 or negative")))
      })


    def parseOneEntry(input: (String, String)): ErrorValidation[User] = {
      val parsedName: ErrorValidation[String] = input._1 |> checkIfEmpty
      val parsedAge: ErrorValidation[Int] = input._2 |> checkIfEmpty |> convertYears |> parseYear
      Applicative[ErrorValidation].apply2(parsedName, parsedAge)((name, age) => User(name, age))
    }

    nameAge.map(parseOneEntry)
  }
}
