package book_advanced_scala.chapter4_monads.each
import com.thoughtworks.each.Monadic._
import scalaz.std.list._
import com.thoughtworks.each.ComprehensionImplicits._

import scala.concurrent.Future
import scalaz.std.option._
import scalaz.std.scalaFuture._
import scalaz._

/**
  * The library Thoughtworks is used to handle monads sequentially.
  */
object Main extends App {
  val result: Option[String] = monadic[Option] {
    "Hello, Each!"
  }

  val name = Option("Each")
  val name2 = Future.successful("some")
  val result2: Option[String] = monadic[Option] {
    "Hello, " + name.each + "!"
  }
    val result3: Future[String] = monadic[Future] {
      "Hello, " + name2.each + "!"
    }
  println(result2)
  result3.map(println)

  val n = Some(10)
  val result4: Option[Int] = monadic[Option] {
    var count = 1
    for (i <- List(300, 20).monadicLoop) {
      count += i * n.each
    }
    count
  }


  val m = Some(4000)
  val result5 = monadic[Option] {
    val f = {
      for {
        i: Int <- List(300, 20).monadicLoop
        (j, k) <- List(50000 -> "1111", 600000 -> "yyy").monadicLoop
        if i > m.each - 3900
        a = i + j
      } yield {
        a + m.each * k.length
      }
    }
  }
}

