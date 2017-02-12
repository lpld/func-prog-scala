package com.github.lpld.study.funcscala.chapter8

import com.github.lpld.study.funcscala.chapter5.Stream
import com.github.lpld.study.funcscala.chapter6.{RNG, SimpleRNG, State}
import com.github.lpld.study.funcscala.chapter8.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

/**
  * @author leopold
  * @since 29/01/17
  */
object Gen {

  def run(p: Prop,
          maxSize: MaxSize = 100,
          testCases: TestCases = 100,
          rng: RNG = new SimpleRNG(System.currentTimeMillis())): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case _ =>
        println(s"+ OK, passed $testCases tests.")
    }
  }

  def listOf[A](a: Gen[A]): SGen[List[A]] = SGen(i => a.listOfN(i))

  def listOf1[A](a: Gen[A]): SGen[List[A]] = SGen(i => a.listOfN(i max 1))

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

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)

  // SuccessCount seems to be incorrect in this method
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + max - 1) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
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
  def map[B](f: A => B): Gen[B] = flatMap(a => Gen.unit(f(a)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f andThen (_.sample)))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap listOfN
  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

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

  protected[chapter8] def &&(another: => Result): Result = this match {
    case Passed => another
    case _ => this
  }

  protected[chapter8] def ||(another: => Result): Result = this match {
    case Passed => this
    case _ => another
  }
}

case object Passed extends Result {
  override def isSatisfied: Boolean = true
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isSatisfied: Boolean = false
}

case class SGen[A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = SGen(forSize andThen (_ map f))

  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(forSize andThen (_ flatMap f))
}
