package book_functional_programming_in_scala.chapter3_FunctionalDataStructures.lists

object EXERCISE3_7 {
  def productEfficientRecursion[A <% Double](list: List[A]): Double = {   //since A <% Double, it will work with anything that can be converted to double , i.e. Int
    list.foldRight(1.0)((left, acc) => left match {
      case 0 => return 0
      case _ => left * acc
    })
  }
}
