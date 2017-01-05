package book_functional_programming_in_scala.chapter6_PurelyFunctionalState

object Generator {
  type Rand[A] = State[RNG, A]      //type Rand[+A] = RNG => (A, RNG)

  def nonNegativeInt: State[RNG, Int] = {
    State{
      rng: RNG =>
        val (num, nextRng) = rng.nextInt
        if (num == Int.MinValue) (0, nextRng)
        else (Math.abs(num), nextRng)
    }
  }

  val int: State[RNG, Int] = State(
    (rng: RNG) =>
      rng.nextInt
  )

  //The generated value will be a double between 0 and 1
  def double: State[RNG, Double] =
    nonNegativeInt.map(_.toDouble / Int.MaxValue)


  def double2(rng: RNG): ((Double, Double), RNG) = {
    val (d1, nextRng1) = double.run(rng)
    val (d2, nextRng2) = double.run(rng)
    ((d1, d2), nextRng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val ((d1, d2), nextRng1) = double2(rng)
    val (d3, nextRng2) = double.run(rng)
    ((d1, d2, d3), nextRng2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @scala.annotation.tailrec
    def generageRandInts(count: Int, rng: RNG, acc: List[Rand[Int]]): List[Rand[Int]] = {
      if (count == 0) acc.reverse
      else {
        val (i, nextRng) = rng.nextInt
        generageRandInts(count - 1, nextRng, State[RNG, Int](rng => rng.nextInt) :: acc)
      }
    }
    State.sequence(generageRandInts(count, rng, Nil))
      .run(rng)

    //    @scala.annotation.tailrec
    //    def iterate(count: Int, accList: List[Int])(rng: RNG): (List[Int], RNG) = {
    //      if(count == 0)
    //        (accList.reverse, rng)
    //      else {
    //        val(i, nextRng) = rng.nextInt
    //        iterate(count - 1, i :: accList)(nextRng)
    //      }
    //    }
    //    iterate(count, Nil)(rng)
  }


  def nonNegativeEven: State[RNG, Int] = nonNegativeInt.map(i => i - i % 2)

  def both[A, B](ra: Rand[A], rb: Rand[B]): State[RNG, (A, B)] = ra.map2(rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    State {
      rng: RNG =>
        val (i, rng2) = nonNegativeInt.run(rng)
        val mod = i % n
        if (i + (n - 1) - mod >= 0)
          (mod, rng2)
        else nonNegativeLessThan(n).run(rng2)
    }
  }

  def nonNegativeLessThan(d: Double): Rand[Double] = {
    State {
      rng: RNG =>
        val (i, rng2) = nonNegativeInt.run(rng)
        val mod = i % d
        if (i + (d - 1) - mod >= 0)
          (mod, rng2)
        else nonNegativeLessThan(d).run(rng2)
    }
  }
}
