package book_functional_programming_in_scala.chapter8_PropertyBasedTesting

import java.util.concurrent.{Executor, ExecutorService, Executors}

import book_functional_programming_in_scala.chapter5_StrictnessAndLaziness.{NonSharedStateWithUnfold, Stream}
import book_functional_programming_in_scala.chapter6_PurelyFunctionalState._
import book_functional_programming_in_scala.chapter7_PurelyFunctionalParallelism.Par.Par
import book_functional_programming_in_scala.chapter8_PropertyBasedTesting.Prop.{Falsified, MaxSize, Passed, Proved, Result, TestCases}


object Prop {
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = List[String]
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(tag: String, failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(tag, msg, n) =>
        println(s"! Falsified after $n passed tests because the test $tag failed:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop("tag",
    (maxSize, n, rng) => {
      randomStream(as)(rng)
        .zipAllWithUnfold(Stream.from(0))
        .takeN(n)
        .map {
          case (Some(a), Some(i)) => try {
            println("check " + a)
            if (f(a)) {
              Passed
            }
            else Falsified("tag", List(a.toString), i)
          } catch {
            case e: Exception => Falsified("tag", List(buildMsg(a, e)), i)
          }
        }.find(_.isFalsified)
        .getOrElse(Passed)
    }
  )

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    NonSharedStateWithUnfold.unfold(rng)(rng => Some(g.sample.run(rng)))
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"



  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop ("tag",
    (max: MaxSize, n: TestCases, rng) => {
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream(
        Stream.from(0)
          .takeN(Math.min(n, max) + 1)
          .map(i => forAll(g(i))(f)):_*
      )
      val prop: Prop =
        props.mapWithUnfold(p => Prop("tag", (max, _, rng) => p.run(max, casesPerSize, rng)))
          .toListFast
          .reduce(_ && _)
      prop.run(max, n, rng)
    }
  )



  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = {
    val S: Gen[ExecutorService] = Gen.weighted(
      Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
      Gen.unit(Executors.newCachedThreadPool()) -> 0.25
    )
    val sGen = SGen(_ => S)

    forAll(sGen ** SGen(_ => g)) {case (s,a) => f(a)(s).get()}
  }

}

case class Prop(tag: String, run: (MaxSize, TestCases, RNG) => Result) {
  def check: Result = ???

  def &&(p: Prop): Prop = Prop(
    this.tag + " + " + p.tag,
    (maxSize, testCases, rng) => run(maxSize, testCases, rng) match {
      case Passed | Proved => p.run(maxSize, testCases, rng)
      case t: Falsified => t
    }
  )

  def ||(p: Prop): Prop = Prop(
    this.tag + " + " + p.tag,
    (maxSize, testCases, rng) =>
      (run(maxSize, testCases, rng), p.run(maxSize, testCases, rng)) match {
        case (false1: Falsified, false2: Falsified) =>
          Falsified(
            this.tag + " + " + p.tag,
            false1.failure ++ false2.failure,
            false1.successes + false2.successes
          )
        case (Proved, Proved) => Proved
        case _ => Passed
    }
  )
}
