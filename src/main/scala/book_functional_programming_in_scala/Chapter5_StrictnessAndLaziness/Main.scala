package book_functional_programming_in_scala.Chapter5_StrictnessAndLaziness
import InfiniteStreams._

object Main extends App {
  println(Stream(1,2,3,4,5).takeWhileLazy(_<3).toListFast)
  println(Stream(1,2,3).headOptionLazy())
  println(Stream().headOptionLazy())
  println(Stream(1,2,3).mapLazy(_ + 1).toListFast)
  println(Stream(1,2,3,4,5).filterLazy(_%2 == 0).toListFast)
  println(Stream(1,2,3).flatMapLazy(x => Stream(x, x + 1)).toListFast)
  println(Stream(1,2,3).appendLazy(4).toListFast)
  println(Stream(1,2,3).appendLazy(List(4,5)).toListFast)
  println(Stream(1,2,3).mapWithUnfold(_ + 1).toListFast)
  val strm1 = Stream(1, 2, 3)
  val strm2 = Stream(4, 5, 6, 7)
  println(strm1.zipAllWithUnfold(strm2).toListFast)
  println(strm2.takeWithUnfold(3).toListFast)
  println(Stream(4, 5, 6, 7).startsWith(Stream(4, 5, 6)))
  println(strm2.tails.mapWithUnfold(_.toListFast).toListFast)
  println(strm1.scanRight(0)(_ + _))
}
