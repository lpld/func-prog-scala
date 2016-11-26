package com.github.lpld.study.funcscala.chapter5

import com.github.lpld.study.funcscala.chapter5.Stream.unfold

/**
  * @author leopold
  * @since 23/10/16
  */
sealed trait Stream[+A] {
  /*
   * 5.1
   */
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  /*
   * 5.2
   */
  def take(n: Int): Stream[A] =
    if (n == 0) Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
    }

  def drop(n: Int): Stream[A] =
    if (n == 0) this
    else this match {
      case Empty => Empty
      case Cons(h, t) => t().drop(n - 1)
    }

  /*
   * 5.3
   */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (p(h())) Cons(h, () => t().takeWhile(p))
      else Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  /*
   * 5.4
   */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  /*
   * 5.5 ???
   */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, t) => if (p(a)) Stream.cons(a, t.takeWhileViaFoldRight(p)) else Empty)

  /*
   * 5.6
   */
  def headOption: Option[A] = foldRight(Option.empty[A])((a, _) => Some(a))

  /*
   * 5.7
   */
  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  //  ((a, t) => Cons(() => f(a), () => t))

  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, t) => if (p(a)) Stream.cons(a, t) else t)

  // not sure about the signature
  def append[AA >: A](that: => Stream[AA]): Stream[AA] = this match {
    case Empty => that
    case Cons(h, t) => Cons(h, () => t().append(that))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, t) => f(a) append t)

  /*
   * 5.13
   */
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), nn) if nn > 0 => Some(h(), (t(), nn - 1))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, that)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def isEmpty: Boolean = this match {
    case Empty => true
    case _ => false
  }

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] = {

    def split[C](str: Stream[C]) = str match {
      case Empty => (None, Empty)
      case Cons(h, t) => (Some(h()), t())
    }

    unfold((this, that)) {
      case (Empty, Empty) => None
      case (s1, s2) =>
        Some(split(s1), split(s2))
          .map { case (spl1, spl2) =>
            (
              (spl1._1, spl2._1),
              (spl1._2, spl2._2)
            )
          }
    }
  }

  /*
   * 5.14
   */
  def startsWith[AA >: A](that: Stream[AA]): Boolean =
    zipAll(that)
      .takeWhile(_._2.nonEmpty)
      .forAll { case (a, aa) => a == aa }



}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /*
   * 5.8
   */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /*
   * 5.9
   */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /*
   * 5.10
   */
  def fibs: Stream[Int] = {
    def ff(v: Int, prev: Int): Stream[Int] = cons(v, ff(v + prev, v))

    ff(0, 1)
  }

  /*
   * 5.11
   * More general stream-building function. It takes an initial state,
   * and a function for producing both the next state and the next value
   * in the generated stream.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case _ => Empty
  }

  /*
   * 5.12
   */
  def fibsViaUnfold: Stream[Int] = unfold((0, 1)) { case (v, prev) => Some((v, (v + prev, v))) }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(0)(_ => Some(a, 0))

}
