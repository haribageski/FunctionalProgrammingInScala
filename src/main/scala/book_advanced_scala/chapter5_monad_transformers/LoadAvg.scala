package book_advanced_scala.chapter5_monad_transformers

import book_advanced_scala.chapter5_monad_transformers.FutureEither.FutureEither
import scalaz.{Applicative, EitherT}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object LoadAvg {
  type FutureEither2[Error, A] = EitherT[Future,Error, A]

  val loadAverages = Map( "a.example.com" -> 0.1, "b.example.com" -> 0.5, "c.example.com" -> 0.2)

  import scalaz.std.scalaFuture._
  import scalaz.syntax.monad._
  import scalaz.syntax.monadError._
  def getLoad(hostname: String): FutureEither[Double] = {
    loadAverages.get(hostname).
    map(_.point[FutureEither]).getOrElse(s"Host unreachable: $hostname".
      raiseError[FutureEither2, Double]
    )
  }

  def getMeanLoad(hostnames: List[String]): FutureEither[Double] = {
    hostnames.foldLeft(0.0.point[FutureEither])((acc, host) =>
      Applicative[FutureEither].apply2(acc, getLoad(host))(_ + _)).
      map(_ / hostnames.size)
  }

  def report[A](input: FutureEither[A]): Unit = {
    Await.result(input.run, 2.seconds).
      bimap(
        e => println(s"[FAIL] $e"),
        meanLoad => println(s"[DONE] $meanLoad")
      )
  }


  def main(args: Array[String]) {
    getLoad("a.example.com").map(println)
    report(getMeanLoad(List("a.example.com", "b.example.com")))
  }
}
