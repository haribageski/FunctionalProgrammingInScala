package book_functional_programming_in_scala.chapter8_PropertyBasedTesting

import book_functional_programming_in_scala.chapter6_PurelyFunctionalState.{Generator, RNG, SimpleRNG, State}
import book_functional_programming_in_scala.chapter7_PurelyFunctionalParallelism.Par
import book_functional_programming_in_scala.chapter7_PurelyFunctionalParallelism.Par.{Par, ParAsync}

case class Gen[A](sample: State[RNG, A]) {

  def map[B](f: A => B) = this.copy(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(i => Gen.listOfN(i, this))

  def listOf1(size: Gen[Int]): Gen[List[A]] = size.flatMap(i =>
    Math.abs(i) match {
      case 0 => listOf1(size)
      case positive => Gen.listOfN(positive, this)
    }
  )

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen.boolean.flatMap {
    case true => g1
    case false => g2
  }

  def unsized: SGen[A] = {
    SGen(_ => this)
  }
}

object Gen {
  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def boolean: Gen[Boolean] =
    Gen(Generator.int.map(_ > 0))

  def int: Gen[Int] = Gen(Generator.int)

  def genOption[A](gen: Gen[A]) = ???

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    @scala.annotation.tailrec
    def iterateRecursively(n: Int, acc: Gen[List[A]]): Gen[List[A]] = {
      n match {
        case i if i <= 0 => acc
        case _ => iterateRecursively(n - 1, Gen(g.sample.map2(acc.sample)(_ :: _)))
      }
    }
    iterateRecursively(n, g.map(_ => Nil))
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(Generator.nonNegativeInt.map(i => start + i % (stopExclusive - start)))
  }

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    Gen(
      choose(start, stopExclusive).sample.map2(choose(start, stopExclusive).sample)((_, _))
    )

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val total = g1._2 + g2._2
    val randState: State[RNG, Double] = Generator.double
    val skewedTo1: State[RNG, Double] = randState.map(_ / total)   //total : random = 1 : x
    val p1 = g1._2 / total    //d1 / total = p1 / 1
    val p2 = g2._2 / total
    Gen(
      skewedTo1.flatMap(_ match {
        case p if(p < p1) => g1._1.sample
        case p if(p <= p2) => g2._1.sample
        case _ =>
          println("Miscalculation of probabilities")
          ???
      })
    )
  }

  def streamInt: Gen[Stream[Int]] = {
    def state(i: Int): Stream[Int] = i match {
      case Int.MaxValue => Stream(i)
      case _            => i #:: state(i + 1)
    }
    Gen.unit(state(Int.MinValue))
  }

  val pint2: Gen[ParAsync[Int]] = choose(-100,100).listOfN(choose(0,20)).map(l =>
    l.foldLeft(Par.unit(0))((acc: ParAsync[Int], i: Int) =>
      Par.fork { Par.map2(acc, Par.unit(i))(_ + _) }
    )
  )


  def genStringFn[A](g: Gen[A]): Gen[(String) => A] = {
    g.flatMap[String => A] { i =>
      Gen(State[RNG, String => A] {
        rng =>
          val (seed: Int, rng2) = rng.nextInt
          val f: (String) => A =
            (s: String) => g.sample.run(SimpleRNG(seed.toLong ^ s.hashCode))._1
          (f, rng2)
      })
      }
  }

  def genIntFn[A](g: Gen[A]): Gen[(Int) => A] = {
    g.flatMap[Int => A] { i =>
      Gen(State[RNG, Int => A] {
        rng =>
          val (seed: Int, rng2) = rng.nextInt
          val f: (Int) => A =
            (s: Int) => g.sample.run(SimpleRNG(seed.toLong ^ s.hashCode))._1
          (f, rng2)
      })
    }
  }
}
