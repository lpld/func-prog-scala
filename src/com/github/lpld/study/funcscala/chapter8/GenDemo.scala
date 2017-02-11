package com.github.lpld.study.funcscala.chapter8

import com.github.lpld.study.funcscala.chapter6.SimpleRNG
import com.github.lpld.study.funcscala.chapter8.Gen.{choose, listOf}

/**
  * @author leopold
  * @since 12/02/17
  */
object GenDemo extends App {

  val listGen: SGen[List[Int]] = listOf(choose(10, 200))

  Gen.forAll(listGen)(_ forall (_ < 180)).run(200, 200, SimpleRNG(System.currentTimeMillis())) match {
    case Passed =>
      println("OK");
    case Falsified(failedCase, succCount) =>
      println(s"Failed: $failedCase after $succCount successful cases")
  }
}
