package book_advanced_scala.chapter5_monad_transformers

import scalaz.OptionT
import scalaz.std.AllInstances._
import scalaz.syntax.monad._

object OptionTransformer extends App{
  type ListOption[A] = OptionT[List, A]
  val result: ListOption[Int] = 42.point[ListOption]
  println("result:" + result)
  val result2: OptionT[List, List[Int]] = OptionT.some(List(42))
  println("result2:" + result2)
  val missing: OptionT[List, Int] = OptionTransformer.missing
  println("missing:" + missing)
}
