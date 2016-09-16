package book_functional_programming_in_scala.chapter6_PurelyFunctionalState

trait RNG {
  def nextInt: (Int, RNG)
}
