package book_advanced_scala.chapter4_monads

import cats.free.Free
import cats.Id
import cats.arrow.NaturalTransformation
import cats.implicits.optionInstance

import scala.util.Try


object FreeMonadEx {
  def getDef(age: Int): String =
    if(age > 18)  "grownup"
    else  "kid"

  sealed trait ConsoleDSL[A]
  case class Ask(question: String) extends ConsoleDSL[String]
  case class AskInt(question: String) extends ConsoleDSL[Int]
  case class Tell(msg: String) extends ConsoleDSL[Unit]

  type ConsoleApp[A] = Free[ConsoleDSL, A]

  def ask(question: String): ConsoleApp[String] =
    Free.liftF(Ask(question))

  def tell(message: String): ConsoleApp[Unit] =
    Free.liftF(Tell(message))

  def askInt(message: String): ConsoleApp[Int] =
    Free.liftF(AskInt(message))

  val program: ConsoleApp[Unit] = for {
    name <- ask("What is your name?")
    age <- askInt("What is your age?")
    _ <- tell(s"Hello $name, you are ${getDef(age.toInt)}")
  } yield ()

  object ConsoleDSLToId extends NaturalTransformation[ConsoleDSL, Id] {
    override def apply[A](fa: ConsoleDSL[A]): Id[A] = fa match {
      case Ask(question) =>
        Console.readLine(question)
      case Tell(message) =>
        println(message)
      case AskInt(question) =>
        Console.readLine(question).toInt
    }
  }

  object ConsoleDSLToOOption extends NaturalTransformation[ConsoleDSL, Option] {
    def apply[A](fa: ConsoleDSL[A]): Option[A] = fa match {
      case Ask(question) =>
        val answer = Console.readLine(question)
        if(answer.isEmpty) None
        else Some(answer)
      case Tell(message) =>
        Some(println(message))
      case AskInt(question) =>
        val answer = Console.readLine(question)
        Try {
          answer.toInt
        }.toOption
    }
  }

  program.foldMap(ConsoleDSLToOOption)

}
