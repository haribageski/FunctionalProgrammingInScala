package book_advanced_scala.chapter4_monads.representCustomError

import cats.data.Xor
import cats.syntax.xor._

trait Result {
  type LoginResult = LoginError Xor User // defined type alias LoginResult

  def handleError(error: LoginError): Unit = error match {
    case UserNotFound(u) => println(s"User not found: $u") case PasswordIncorrect(u) => println(s"Password incorrect: $u") case _ : UnexpectedError => println(s"Unexpected error")
  }
  // handleError: (error: LoginError)Unit
  val result1: LoginResult = User("dave", "passw0rd").right // result1: LoginResult = Right(User(dave,passw0rd))
  val result2: LoginResult = UserNotFound("dave").left // result2: LoginResult = Left(UserNotFound(dave))
  result1.fold(handleError, println) // User(dave,passw0rd)
  result2.fold(handleError, println) // User not found: dave
}

case class User(username: String, password: String) // defined class User
sealed trait LoginError
// defined trait LoginError
final case class UserNotFound(username: String) extends LoginError // defined class UserNotFound
final case class PasswordIncorrect(username: String) extends LoginError // defined class PasswordIncorrect
trait UnexpectedError extends LoginError // defined trait UnexpectedError
