package book_advanced_scala.chapter7_Hadoop_Case_Study

import java.util.Calendar
import java.util.concurrent.TimeUnit

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.parallel.immutable.ParSeq
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scalaz._
import scalaz.Scalaz._
import scala.concurrent.duration.Duration
import org.scalameter._

object ParallelFoldMap {

  var switch = false
  println("----------")
  val processors: Int =  Runtime.getRuntime.availableProcessors
  println("processors:" + processors)

  def foldMap[A, B: Monoid](values: Iterable[A])(func: A => B): B = {
    values.map(func)
      .fold(mzero[B])(Monoid[B].append(_, _))
  }

  def foldMapP[A, B : Monoid](values: Iterable[A])(func: A => B)(implicit ec: ExecutionContext): B = {
    if(values.size < 20)
      foldMap(values)(func)
    else {
      val toExecInParallel = values.grouped((values.size.toDouble / processors).toInt + 1).toSeq.par
      toExecInParallel.map(x => foldMap(x)(func))
        .fold(mzero[B])(Monoid[B].append(_, _))
    }
  }

  def foldMapF[A, B : Monoid](values: Iterable[A])(func: A => B)(implicit ec: ExecutionContext): Future[B] = {
    if(values.size < 10)
      Future(foldMap(values)(func))
    else {
      Future.sequence{
        values.grouped((values.size.toDouble / processors).toInt + 1)
          .map(iterable => Future(foldMap(iterable)(func)))
      }.map(_.fold(mzero[B])(Monoid[B].append(_, _)))
    }
  }
}

object FoldMapRunner extends App {
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  val listToTest = Range(1, 100000).toList

  val futureTime = standardConfig measure {
    ParallelFoldMap.foldMapF(listToTest)(x => List(x))
  }
  println(s"future time: $futureTime ms")

//  val parallelTime = standardConfig measure {
//    ParallelFoldMap.foldMapP(listToTest)(x => List(x)) //have implicit DefaultMonoidsAddition
//  }
//  println(s"parallel time: $parallelTime ms")

//  val seqtime = standardConfig measure {
//    ParallelFoldMap.foldMap(listToTest)(x => List(x)) //have implicit DefaultMonoidsAddition
//  }
//  println(s"sequential time: $seqtime ms")

  //    val currentSeconds = Calendar.getInstance().get(Calendar.SECOND)
  //
}

