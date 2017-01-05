import cats.data.{Kleisli, Xor}
import cats.syntax.xor._
import org.scalatestplus.play.PlaySpec

import scalaz.Semigroup // For .left and .right

class Test extends PlaySpec {
  type Result[A] = Xor[List[String],A]
  type CheckK[A,B] = Kleisli[Result,A,B]
  // This constructor helps with type inference
  def Check[A,B](f: A => Result[B]): CheckK[A, B] =
    Kleisli[Result, A, B](f)

  val checkNonNegative: CheckK[Int, Int] =
    Check((x: Int) => {
      if (x >= 0) x.right
      else List("Error in conversion").left
    })

  val checkBoundary =
    Check((x: Int) => {
      if (x < 10) x.right
      else List("Error limit reached").left
    })

  "checkNonNegative" should {
    "be right" in {
      checkNonNegative.run(1) mustBe (1.right)
    }
  }
  it should {
    "be left" in {
      checkNonNegative.run(-8) mustBe (List("Error in conversion").left)
    }
  }

  "checkBoundary" should {
    "be right" in {
      checkBoundary.run(3) mustBe (3.right)
    }
  }
  it should {
    "be left" in {
      checkBoundary.run(10) mustBe (List("Error limit reached").left)
    }
  }


  import cats.std.list._
  type listOfStrings = List[String]
  implicit val semigroup: Semigroup[listOfStrings] = new Semigroup[listOfStrings] {
    override def append(f1: listOfStrings, f2: => listOfStrings): listOfStrings= f1 ++ f2
  }

  "checkNonNegative and checkBoundary" should {
    "be right" in {
      (checkNonNegative andThen  checkBoundary).run(3)  mustBe (3.right)
    }
  }
  it should {
    "be left" in {
      (checkNonNegative andThen  checkBoundary).run(-8) mustBe (List("Error in conversion").left)
    }
  }

  it should {
    "be list of two left" in {
      (checkNonNegative andThen  checkBoundary).run(10) mustBe (List("Error limit reached").left)
    }
  }


}

