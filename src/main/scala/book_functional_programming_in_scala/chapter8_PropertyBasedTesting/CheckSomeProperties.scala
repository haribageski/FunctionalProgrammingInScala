package book_functional_programming_in_scala.chapter8_PropertyBasedTesting

import book_functional_programming_in_scala.chapter6_PurelyFunctionalState.Generator.int
import SGen._
import Prop._
import book_functional_programming_in_scala.chapter3_FunctionalDataStructures.trees.{Branch, Leaf, Tree}
import book_functional_programming_in_scala.chapter6_PurelyFunctionalState.{RNG, SimpleRNG}
import book_functional_programming_in_scala.chapter7_PurelyFunctionalParallelism.Par._
import org.scalacheck.rng.Seed

object CheckSomeProperties extends App{

  val maxProp1: Prop = forAll(listOf1(Gen(int))){ l =>
    println("list:" + l)
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }
  println("maxProp1:" + maxProp1.run(100, 2, SimpleRNG(System.currentTimeMillis)))

  val sortedListProp1 = {
    forAll(listOf1(Gen(int))) { l =>
      (l.head :: l.tail.sorted).sorted == l.sorted
    } && forAll(listOf1(Gen(int))) { l =>
      l.size <= 1 || Math.max(l.head, l.tail.sorted.reverse.head) == l.sorted.reverse.head
    } && forAll(listOf1(Gen(int))) { l =>
      val sorted = l.sorted
      sorted.forall(l.contains(_)) && l.forall(sorted.contains(_))
    }
  }
  println("sortedListProp1:" + sortedListProp1.run(100, 10, SimpleRNG(System.currentTimeMillis)))

//  val pint: Gen[ParAsync[SuccessCount]] = Gen.choose(0, 10).map(unit)

//  val forkProp = Prop.forAllPar(Gen.pint2)(i => Par(equal(ExecutorService) (Par.fork(i), i))) tag "fork"

//  val forkProp = forAllPar[ParAsync[Int]](Gen.pint2)((pint: ParAsync[Int]) =>
//    equal(ExecutorService)(Par.fork(pint), pint))
//  )


  val listProp = {
    val listAndTakeNsGen: SGen[(List[Int], Int)] = listOf1(Gen.int) ** SGen(_ => Gen.int)
    val listAndPredicate: SGen[(List[Int], (Int) => Boolean)] =
      listOf1(Gen.int) ** SGen(_ => Gen.genIntFn(Gen.boolean))

    forAll[(List[Int], Int)](listAndTakeNsGen)(listAndN => {    //drop and take property
      val list = listAndN._1
      val n = Math.min(listAndN._2, list.size)
      list.take(n) ::: list.drop(n) == list
    }) &&
    forAll(listAndPredicate)((listWithPredicate) => {   //filter property
      listWithPredicate._1.filter(listWithPredicate._2) == (
        listWithPredicate._2(listWithPredicate._1.head) match {
        case true  => listWithPredicate._1.head :: listWithPredicate._1.tail.filter(listWithPredicate._2)
        case false => listWithPredicate._1.tail.filter(listWithPredicate._2)
      })
    })
  }
  println("listProp:" + listProp.run(100, 10, SimpleRNG(System.currentTimeMillis)))


  val treeProp = {
    forAll(SGen.tree(Gen.int) ** SGen(_ => Gen.int)) ((treeAndInitial: (Tree[Int], Int)) => {
      val recursiveAcc: Int = treeAndInitial._1 match {
        case branch: Branch[Int] =>
          val nextAcc = branch.getVal + treeAndInitial._2
          branch.left.foldTree[Int](nextAcc)(_ + _, _ + _) + branch.right.foldTree(nextAcc)(_ + _, _ + _)
        case Leaf(a) => a + treeAndInitial._2
      }
      recursiveAcc== treeAndInitial._1.foldTree(treeAndInitial._2)(_ + _, _ + _)
    })
  }
  println("treeProp:" + treeProp.run(100, 10, SimpleRNG(System.currentTimeMillis)))
}
