## Functional Structures 

(https://github.com/haribageski/FunctionalProgrammingInScala.git)



- ### Either

  Doesn’t have a map method, both sides are equal (non-biased)

- ### Disjunction \/

Right biased (has map on the right part)

Use .merge() to shift from \/[A, A] to just A
Use option.toRightDisjunction(“error msg”) to convert an option to a disjunction, or do the same in a shorter way: option \/> “error msg”


One can create a disjunction as follows:

```
"Cat".parseInt.disjunction
// res8: scalaz.\/[NumberFormatException,Int] = ↩
// -\/(java.lang.NumberFormatException: For input string: "Cat")

"1".parseInt.disjunction
// res9: scalaz.\/[NumberFormatException,Int] = \/-(1)
```

- ### Monoid

Def. A monoid for a type A is:

• an operation 'combine' with type (A,A)=>A;
• a unit element of type A, called 'empty'.

The following laws must hold:
• combine(x, combine(y, z)) == combine(combine(x, y), z) for all x,y, and z, in A;
• combine(a, empty) == combine(empty, a) == a for any a in A.

Note: Available by importing the library cats.Monoid


Ex. 

```
implicit val monoid: Monoid[Order] = new Monoid[Order]{

  override def empty: Order = Order(0, 0)

  override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)

}
```

- ### Functor

• start with F[A];

• supply a function A=>B; 

• get back F[B].

Ex. Functions (it is just function composition):

• start with R => A;

• supply a function A=>B; 

• get back R => B.

Formal Definition:

    trait Functor[F[_]] {   //type class that abstracts over type constructor, or type constructor of one argument
    
      def mapA, B(f: A => B): F[B]
    
    }
    
    trait FunctorLaws{
    
      def identityF[_], A(implicit F: Functor[F]) =
    F.map(fa)(a => a) == fa
    def composition[F[_], A, B, C](fa: F[A], f: A => B, g: B => C)(implicit F: Functor[F]) =
        F.map(F.map(fa)(f))(g) == F.map(fa)(f andThen g)
Usage:			
cats.Functor.



- ### Monad Transformers

  - `Future[String \/ Int] -> EitherT[Future, String, Int]`
  - `type HttpResult[A] = EitherT[Future, Result, A]`

  Ex.

  ```
  val z1: OptionT[Future, Int] = for {
   x <- OptionT(getX) 
   y <- OptionT(getY) 
  } yield x + y 
  val z: Future[Option[Int]] = z1.run
  ```

  - Multiple containers types problem:

    ```
    //this doesn’t work
    def fa: Option [Int]
    def fb: Strubg \/ Int
    for{
    a <- fa
    b <- fb
    } yield a + b // is it Option[Int] or \/
    ```

- ### Reader Monad

  Reduce dependencies to only the most primitive methods. Higher level classes compose reader monads and don’t change when the interface of a primitive class changes.

  http://blog.originate.com/blog/2013/10/21/reader-monad-for-dependency-injection/



- ### Kleisli

  The abstract concept of composing func ons of type A => F[B] has a name: a Kleisli.

  Ex. 

  ```
  object kleisli {
  import cats.data.Kleisli import cats.std.list._

  // To make the code more concise
  // A Kleisli that transforms an Int to a List[Int] type ToListT[A,B] = Kleisli[List,A,B]

  // Define a few functions that transform an integer to a list of integers

  val incrementAndDecrement: ToListT[Int,Int] = Kleisli(x => List(x + 1, x - 1))

  val doubleAndHalve: ToListT[Int,Int] = Kleisli(x => List(x * 2, x / 2))

  val valueAndNegation: ToListT[Int,Int] = Kleisli(x => List(x, -x))

    // Compose into a transformation pipeline
  val pipeline = incrementAndDecrement andThen valueAndNegation andThen doubleAndHalve

    // Apply the pipeline to data
  val result = pipeline.run(20) }

  // defined object kleisli
  kleisli.result

  // res0: List[Int] = List(42, 10, -42, -10, 38, 9, -38, -9)

  ```



- ### Free Monads

Useful when we want to unite and wrap different case classes  into a single Monad.
A typical Free structure might look like:

Suspend(F(Suspend(F(Suspend(F(....(Pure(a))))))))

Concretely, it is just a clever construction that allows us to build a very simple Monad from any functor.

The above forgetful functor takes a Monad and:

forgets its monadic part (e.g. the flatMap function)
forgets its pointed part (e.g. the pure function)
finally keeps the functor part (e.g. the map function)
By reversing all arrows to build the left-adjoint, we deduce that the free monad is basically a construction that:

takes a functor
adds the pointed part (e.g. pure)
adds the monadic behavior (e.g. flatMap)
In terms of implementation, to build a monad from a functor we use the following classic inductive definition:

sealed abstract class Free[F[_], A]
case class Pure[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](a: F[Free[F, A]]) extends Free[F, A]



Side note: The free monoid will wrap an arbitrary type and must itself be a monoid. 

Sources:
http://typelevel.org/cats/tut/freemonad.html <- the best
http://timperrett.com/2013/11/21/free-monads-part-1/
Videos:
https://www.youtube.com/watch?v=hmX2s3pe_qk
https://www.youtube.com/watch?v=M258zVn4m2M
https://www.youtube.com/watch?v=U0lK0hnbc4U



- ### FS2 Functional Streams

  - Constructor: fs2Stream[Task, Path](collection : _*)  //collection can be a Strea, an Array, a List, etc.
  - Pipe: Incoming element -> Outgoing element + some effect during the conversion. 
    operator ‘>>’ used instead of flatMap(_ => …)  , that is, when we disregard the input of a flatMap.
  - Handle and Pull - usage in implementing custom Pipes
  - Stream.Handle.receive1  extracts 1 element and a Handler for the tail.
    a #: h == Step(a, h)
    Stream.Handle.receive  extracts a chunk of elements.
  - Chunkiness improves performance , so we should consider buffering a data source into chunks of 100 elements before applying Stream.
  - Pure Stream to Effectful Stream (with Effect - Task): 	stream.covary[Task]
    Stream to Pull: stream.open
    Pull to Stream: stream.close	
  - Map over the effect: stream.evalMap{ i => Task(println(i)) } : Stream[Task, Unit]
  - fs2.Scheduler: 
    Define implicit val scheduler = Scheduler.fromFixedDaemonPool(2)
    To cancel a stream based program: stream.interruptWhen
  - Pulling

  Ex.

  ```
  def unlockKeystores(knownPasswords: Stream[Password])(keystores: fs2Stream[Task, Path]): fs2Stream[Task, KeyStore] = {

      def openKeystoresHelper(): (Handle[Task, Path], Stream[Password]) => Pull[Task, Option[KeyStore], Nothing] = {
        (newHandle: Handle[Task, Path], knownPasswords: Stream[Password]) =>
          val nextPull: Pull[Task, Option[KeyStore], (Handle[Task, Path], Stream[Password])] = for {
            (keystorePath: Path, newHandle: Handle[Task, Path]) <- newHandle.await1
            keystoreWithPasswordO: Option[(KeyStore, Password)] <- unlockKeystore(keystorePath, streamToFS2(knownPasswords))
            passwordO: Option[Password] = keystoreWithPasswordO.map(_._2)
            _ <- Pull.output1(keystoreWithPasswordO.map(_._1))
          } yield (newHandle, passwordO.map(_ #:: knownPasswords).getOrElse(knownPasswords))

          nextPull.flatMap((nextHandleAndPasswords: (Handle[Task, Path], Stream[Password])) =>
            openKeystoresHelper()(nextHandleAndPasswords._1, nextHandleAndPasswords._2)
          )
      }

      keystores.pull[Task, Option[KeyStore]] {
        (handle: Handle[Task, Path]) => openKeystoresHelper()(handle, knownPasswords)
      }.collect[KeyStore] {
        case Some(openedKeystore) => openedKeystore
      }
    }
  ```