package com.github.lpld.study.funcscala.chapter10

import com.github.lpld.study.funcscala.chapter7.Par
import com.github.lpld.study.funcscala.chapter7.Par.Par
import com.github.lpld.study.funcscala.chapter8.Gen.{forAll, listOf}
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

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  /*
   * 10.5. Implement `foldMap`
   */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  /*
   * 10.6. Implement `foldLeft` using `foldMap`
   */
  def foldLeftViaFoldMap[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => f(_, a))(z)

  /*
   * 10.7. Implement a `foldMap` for `IndexedSeq` using balanced fold strategy
   */
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v match {
    case IndexedSeq() => m.zero
    case IndexedSeq(a) => f(a)
    case _ =>
      val (fst, snd) = v.splitAt(v.length / 2)
      m.op(foldMapV(fst, m)(f), foldMapV(snd, m)(f))
  }

  /*
   * 10.8. Implement a parallel version of `foldMap`.
   */
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = Par.fork(Par.map2(a1, a2)(m.op))

    val zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(f andThen Par.unit)

  /*
   * 10.9. Use `foldMap` to detect whether a given `IndexedSeq[Int]` is ordered.
   */
  def isOrdered(v: IndexedSeq[Int]): Boolean = {
    case class SeqInfo(ordered: Boolean,
                       order: Int = 0, // <0 means descending, >0 - ascending, =0 - unknown
                       boundaries: Option[(Int, Int)] = None)

    val m = new Monoid[SeqInfo] {
      def op(s1: SeqInfo, s2: SeqInfo): SeqInfo = (s1.boundaries, s2.boundaries) match {
        // SeqInfo with no boundaries is itself ordered and has no effect to the result of `op`
        case (None, _) => s2
        case (_, None) => s1
        // if both SeqInfos are non-empty, we have to merge them:
        case (Some((left1, right1)), Some((left2, right2))) =>
          val newBoundaries = Some((left1, right2))
          // check, whether one of the intervals is not ordered, or the orders differ:
          if (!s1.ordered || !s2.ordered || s1.order * s2.order < 0)
            SeqInfo(ordered = false, boundaries = newBoundaries)
          else {
            // calculate the sign of the difference
            def sign(i: Int) = if (i < 0) -1 else if (i == 0) 0 else 1

            val diffSign = sign(left2 - right1)

            val expectedOrder = if (s1.order == 0) s2.order else s1.order
            val ordered = diffSign * expectedOrder >= 0
            val order = if (expectedOrder == 0) diffSign else expectedOrder

            SeqInfo(
              ordered = ordered,
              order = order,
              boundaries = newBoundaries)
          }
      }

      val zero: SeqInfo = SeqInfo(ordered = true)
    }

    foldMapV(v, m)(i => SeqInfo(ordered = true, 0, Some((i, i)))).ordered
  }

  def isOrderedTest: Prop = {
    val list = listOf(Gen.int).map(_.toIndexedSeq)
    val ascending = list.map(_.sorted)
    val descending = ascending.map(_.reverse)

    def checkOrder(seq: IndexedSeq[Int], asc: Boolean): Boolean =
      if (seq.isEmpty) true
      else seq.zip(seq.drop(1)).forall { case (a, b) => (asc && a <= b) || (!asc && a >= b) }

    forAll(ascending)(isOrdered) &&
      forAll(descending)(isOrdered) &&
      forAll(list)(l => isOrdered(l) == checkOrder(l, asc = true) || checkOrder(l, asc = false))
  }

  // Parallel parsing
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  /*
   * 10.10. Write a monoid instance for `WC` and make sure it meets the monoid laws.
   */
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
      case (Stub(c1), Part(ls, count, rs)) => Part(c1 + ls, count, rs)
      case (Part(ls, count, rs), Stub(c2)) => Part(ls, count, rs + c2)
      case (Part(ls1, count1, rs1), Part(ls2, count2, rs2)) =>
        val extraWord = if (rs1.nonEmpty || ls2.nonEmpty) 1 else 0
        Part(ls1, count1 + count2 + extraWord, rs2)
    }
    def zero: WC = Part("", 0, "")
  }

  /*
   * 10.11. Implement a function that counts words in a String
   */
  def wordsCount(input: String): Int = {
    val result = foldMapV(input, wcMonoid)(char => if (char.isWhitespace) wcMonoid.zero else Stub(char.toString))
    def countW(s: String) = if (s.isEmpty) 0 else 1

    result match {
      case Stub(string) => countW(string)
      case Part(l, count, r) => count + countW(l) + countW(r)
    }
  }

  /*
   * 10.16. Monoid product
   */
  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(a1: (A, B), a2: (A, B)): (A, B) = (a.op(a1._1, a2._1), b.op(a1._2, a2._2))

      val zero: (A, B) = (a.zero, b.zero)
    }

  /*
   * 10.17. Write a monoid instance for functions whose results are monoids
   */
  def functionMonoid[A, B](mb: Monoid[B]): Monoid[A => B] =
    new Monoid[(A) => B] {
      def op(a1: A => B, a2: A => B): A => B = a => mb.op(a1(a), a2(a))

      val zero: A => B = _ => mb.zero
    }

  /*
   * 10.18. Use monoids to construct a bag from IndexedSeq
   */
  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    val zero: Map[K, V] = Map[K, V]()

    def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero),
                            b.getOrElse(k, V.zero)))
      }
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}
