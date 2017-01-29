package com.github.lpld.study.funcscala.chapter8

import com.github.lpld.study.funcscala.chapter6.{RNG, State}
import com.github.lpld.study.funcscala.chapter8.Prop.{FailedCase, SuccessCount}

/**
  * @author leopold
  * @since 29/01/17
  */
object Gen {

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(a.sample)))

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(RNG.chooseInt(_)(start, stopExclusive)))

  def boolean: Gen[Boolean] = Gen(State[RNG, Int](_.nextInt).map(_ % 2 == 0))
}

case class Gen[A](sample: State[RNG, A])

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {

//  def &&(p: Prop): Prop = new Prop {
//    override def check: Boolean = Prop.this.check && p.check
//  }

  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}
