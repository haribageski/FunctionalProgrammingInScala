package book_functional_programming_in_scala.chapter7_PurelyFunctionalParallelism

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, TimeUnit, Future => JavaFuture}

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props

import scala.concurrent.{CanAwait, ExecutionContext, TimeoutException}
import scala.concurrent.duration.{Duration, TimeUnit}
import scala.util.Try

object Par {
  sealed trait Future[A] {
    def apply(k: A => Unit, e: Throwable => Unit): Unit
  }

  type Par[A] = ExecutorService => JavaFuture[A]
  type ParAsync[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends JavaFuture[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isCancelled: Boolean = false
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def isDone: Boolean = true
  }

  case class Map2Future[A, B, C](a: JavaFuture[A], b: JavaFuture[B],
                                 f: (A, B) => C) extends JavaFuture[C] {
    @volatile var cache: Option[C] = None
    def get(): C = compute(Long.MaxValue)
    def get(timeout: Long, unit: TimeUnit): C = compute(timeout)

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime
        val aTime = stop - start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)
    override def isCancelled: Boolean = a.isCancelled || b.isCancelled
    override def isDone: Boolean = a.isDone && b.isDone
  }

  /**
    * Implementation via Actors. It works for bounded thread pool.
    */
  def map2UsingActors[A,B,C](p: ParAsync[A], p2: ParAsync[B])(f: (A,B) => C): ParAsync[C] =
    es => new Future[C] {
      override def apply(cb: C => Unit, e: (Throwable) => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None

        class MyActor extends Actor {
          override def receive: Receive = {
            case Left(a: A) => br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(cb(f(a, b)))
            }
            case Right(b: B) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(f(a, b)))
            }
        }
        }
        val system = ActorSystem("HelloSystem")
        val actorinstance = system.actorOf(Props[MyActor])

        p(es)(a => actorinstance ! Left(a), ???)
        p2(es)(b => actorinstance ! Right(b), ??? )
      }
    }

  def unit[A](a: A): ParAsync[A] =
    es => new Future[A] {
      def apply(cb: A => Unit, e: Throwable => Unit): Unit = cb(a)
    }

  /**
    * eval forks off evaluation of a and returns immediately.
    * The callback will be invoked asynchronously on another thread.
    */
  def fork[A](a: => ParAsync[A]): ParAsync[A] =
    es => new Future[A] {
      def apply(cb: A => Unit, e: Throwable => Unit): Unit =
        eval(es)(a(es)(cb, e))
  }

  /**
    * A helper function to evaluate an action asynchronously using some ExecutorService.
    */
  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  def lazyUnit[A](a: => A): ParAsync[A] = fork(unit(a))

  /**
    * run blocks the calling thread while waiting for the latch.
    */
  def run[A](es: ExecutorService)(p: ParAsync[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es)(
      a => {
        ref.set(a)
        latch.countDown
      },
      e => latch.countDown
    )
    latch.await
    ref.get
  }

  def asyncF[A, B](f: A => B): A => ParAsync[B] = {
    a: A => lazyUnit(f(a))
  }

  def map[A, B](pa: ParAsync[A])(f: A => B): ParAsync[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def parMap[A, B](ps: List[A])(f: A => B): ParAsync[List[B]] = {
    val fbs: List[ParAsync[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[ParAsync[A]]): ParAsync[List[A]] = {
    ps.foldLeft(lazyUnit(List.empty[A]))((acc: ParAsync[List[A]], par: ParAsync[A]) => {
      map2(acc, par)((list, a) => a :: list)
    })
  }

  def parFilter[A](as: List[A])(f: A => Boolean): ParAsync[List[A]] = {
    as.foldLeft(unit(List.empty[A]))((acc: ParAsync[List[A]], a: A) => {
      if (f(a)) map(acc)(a :: _)
      else map(acc)(list => list)
    })
  }

  def parMax[A](seq: IndexedSeq[A])(implicit order : Ordering[A]): ParAsync[A] = {
    if(seq.size < 2)  lazyUnit(seq.max)
    else {
      val (a, b) = seq.splitAt(seq.size / 2)
      val aMax: ParAsync[A] = parMax(a)
      val bMax: ParAsync[A] = parMax(b)
      map2(aMax, bMax)(order.max)
    }
  }

  def totalWords(paragraphs: List[String]) = {
    def totalWords(paragraph: String): Int = {
      paragraph.split(" ").length
    }
    parMap(paragraphs)(totalWords)
  }

  def map3[A, B, C, D](a: ParAsync[A], b: ParAsync[B], c: ParAsync[C])(f: (A, B, C) => D): ParAsync[D] = {
    val ab: ParAsync[(A, B)] = map2(a, b)((_, _))
    map2(ab, c)((p, c)=> f(p._1, p._2, c))
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  def choiceN[A](n: ParAsync[Int])(choices: List[ParAsync[A]]): ParAsync[A] = {
    es =>
      var index = 0
      n(es)(index = _, println)
      choices(index)(es)
  }

  def flatMap[A,B](a: ParAsync[A])(f: A => ParAsync[B]): ParAsync[B] = {
    val parF: (ExecutorService) => (A) => ParAsync[B] = (es: ExecutorService) => f
    es =>
      parF(es)(run(es)(a))(es)
  }

  def join[A](a: => ParAsync[ParAsync[A]]): ParAsync[A] = {
    es =>
      run(es)(a)(es)
  }

  def joinViaFlatMap[A](a: ParAsync[ParAsync[A]]): ParAsync[A] = {
    flatMap(a)(p => p)
  }

  def map2[A,B,C](p: ParAsync[A], p2: ParAsync[B])(f: (A,B) => C): ParAsync[C] = {
    flatMap(p)(a =>
      flatMap(p2)(b => unit(f(a, b)))
    )
  }
}
