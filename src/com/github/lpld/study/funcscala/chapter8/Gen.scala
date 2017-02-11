package com.github.lpld.study.funcscala.chapter8

import com.github.lpld.study.funcscala.chapter5.Stream
import com.github.lpld.study.funcscala.chapter6.{RNG, State}
import com.github.lpld.study.funcscala.chapter8.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

/**
  * @author leopold
  * @since 29/01/17
  */
object Gen {

  def listOf[A](a: Gen[A]): SGen[List[A]] = SGen(unit[Int] andThen a.listOfN)

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(a.sample)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(RNG.chooseInt(_)(start, stopExclusive)))

  def boolean: Gen[Boolean] = Gen(State[RNG, Int](_.nextInt).map(_ % 2 == 0))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen(State(RNG.boolean)).flatMap(if (_) g1 else g2)

  def weighted[A](g1: (Double, Gen[A]), g2: (Double, Gen[A])): Gen[A] =
    Gen(State(RNG.weighted(g1._1, g2._1)))
      .flatMap(if (_) g1._2 else g2._2)

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = flatMap(f andThen Gen.unit)

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f andThen (_.sample)))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap (Gen.listOfN(_, this))

  def unsized: SGen[A] = SGen(_ => this)
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = combine(p)(_ && _)

  def ||(p: Prop): Prop = combine(p)(_ || _)

  private def combine(p: Prop)(c: (Result, => Result) => Result) = Prop {
    (max, tc, rng) => c(this.run(max, tc, rng), p.run(max, tc, rng))
  }
}

sealed trait Result {
  def isSatisfied: Boolean

  def isFalsified: Boolean = !isSatisfied

  protected[chapter8] def &&(another: => Result): Result

  protected[chapter8] def ||(another: => Result): Result
}

case object Passed extends Result {
  override def isSatisfied: Boolean = true

  override protected def &&(another: => Result): Result = another

  override protected def ||(another: => Result): Result = this
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isSatisfied: Boolean = false

  override protected def &&(another: => Result): Result = this

  override protected def ||(another: => Result): Result = another
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = SGen(forSize andThen (_ map f))

  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(forSize andThen (_ flatMap f))
}