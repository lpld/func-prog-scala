package com.github.lpld.study.funcscala.chapter5

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
  foldRight(Stream.empty[A])((a, t) => if (p(a)) Cons(() => a, () => t.takeWhileViaFoldRight(p)) else Empty)

  /*
   * 5.6
   */
  def headOption: Option[A] = foldRight(Option.empty[A])((a, _) => Some(a))

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
}
