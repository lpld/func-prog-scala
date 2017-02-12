package com.github.lpld.study.funcscala.chapter8

import com.github.lpld.study.funcscala.chapter6.{SimpleRNG, State}
import com.github.lpld.study.funcscala.chapter8.Gen.{choose, forAll, listOf}

/**
  * @author leopold
  * @since 12/02/17
  */
object GenDemo extends App {

  val listGen: SGen[List[Int]] = listOf(choose(10, 200))

  forAll(listGen)(_ forall (_ < 180)).run(10, 200, SimpleRNG(System.currentTimeMillis())) match {
    case Passed =>
      println("OK");
    case Falsified(failedCase, succCount) =>
      println(s"Failed: $failedCase after $succCount successful cases")
  }

  val anyList: SGen[List[Int]] = Gen.listOf1(Gen(State(_.nextInt)))
  val sortedList = anyList.map(_.sorted)

  val sortedProp = forAll(sortedList)(l => l.zip(l.tail) forall { case (i1, i2) => i1 < i2 })

  Gen.run(sortedProp)
}
