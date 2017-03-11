package com.github.lpld.study.funcscala.chapter10

import com.github.lpld.study.funcscala.chapter8.Gen.forAll
import com.github.lpld.study.funcscala.chapter8.{Gen, Prop}

/**
  * @author leopold
  * @since 11/03/17
  */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val zero = Nil
  }

  /*
   * 10.1. Give `Monoid` instances for integer addition and multiplication
   * as well as the Boolean operators
   */
  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    val zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val zero: Boolean = true
  }

  /*
   * 10.2. Give a `Monoid` instance for combining `Option` values
   */
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    val zero = None
  }

  /*
   * 10.3. Write a `Monoid` for endofunctions
   */
  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
    val zero: (A => A) = identity
  }

  /*
   * 10.4. Use the property-based testing framework to implement a
   * property for the monoid laws.
   */
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val triples = gen.listOfN(3).map { case a :: b :: c :: Nil => (a, b, c) }

    import m._
    val zeroProp = forAll(gen)(a => op(zero, a) == a && op(a, zero) == a)
    val assocProp = forAll(triples) { case (a, b, c) => op(op(a, b), c) == op(a, op(b, c)) }

    zeroProp && assocProp
  }
}

