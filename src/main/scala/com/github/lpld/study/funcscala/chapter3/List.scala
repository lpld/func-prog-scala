package com.github.lpld.study.funcscala.chapter3

import scala.annotation.tailrec

/**
  * @author leopold
  * @since 23/08/16
  */
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](elem: A, tail: List[A]) extends List[A]

object List {

  def apply[A](vals: A*): List[A] =
    if (vals.isEmpty) Nil
    else Cons(vals.head, apply(vals.tail: _*))

  def tail[A](list: List[A]) = list match {
    case Nil => throw new Error("asd")
    case Cons(x, xs) => xs
  }

  @tailrec
  def drop[A](list: List[A], n: Int): List[A] =
    if (n == 0) list
    else list match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }

  @tailrec
  def dropWhile[A](list: List[A])(f: A => Boolean): List[A] =
    list match {
      case Nil => Nil
      case l@Cons(x, xs) =>
        if (f(x)) dropWhile(xs)(f) else l;
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(x, xs) => Cons(x, append(xs, a2))
    }

  def init[A](l: List[A]): List[A] = l match {
    //    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }


  /*
   3.9
   */
  def length(l: List[_]) = foldRight(l, 0)((_, sum) => sum + 1)

  /*
   3.10
   */
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /*
   3.11
   */
  def sum(ints: List[Int]) = foldLeft(ints, 0)(_ + _)

  def product(ints: List[Int]) = foldLeft(ints, 1)(_ * _)

  /*
   3.12
   */
  def reverse1[A](original: List[A]) = {
    def loop(list: List[A], collected: List[A]): List[A] = list match {
      case Nil => collected
      case Cons(x, xs) => loop(xs, Cons(x, collected))
    }

    loop(original, Nil)
  }

  def reverse[A](list: List[A]) = foldLeft(list, Nil: List[A])((a, b) => Cons(b, a))


  //  /*
  //   3.13
  //   */
  //  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
  //  foldRight(l, z)((a, b) => {
  //
  //
  //  })
  // ?????
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((b, a) => f(a, b))

  /*
   3.14
   */
  def append2[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(Cons(_, _))

  /*
   3.15
   */
  def concat[A](superList: List[List[A]]): List[A] = foldRight(superList, Nil: List[A])(append2)

  /*
   3.16
   */
  def addOne(l: List[Int]) = foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))

  /*
   3.17
   */
  def toStrings(l: List[Double]) = foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))

  /*
   3.18
   */
  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))

  /*
   3.19
   */
  def filter[A](l: List[A])(f: A => Boolean) = foldLeft(l, Nil: List[A])((b, a) => if (f(a)) Cons(a, b) else b)

  /*
   3.20
   */
  def flatMap[A, B](l: List[A])(f: A => List[B]) = foldLeft(l, Nil: List[B])((b, a) => append(b, f(a)))

  /*
   3.21
   */
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean) = flatMap(l)(el => if (f(el)) List(el) else Nil)

  /*
   3.22
   */
  def addLists(l1: List[Int], l2: List[Int]): List[Int] = l1 match {
    case Nil => Nil
    case Cons(x1, xs1) => l2 match {
      case Nil => Nil
      case Cons(x2, xs2) => Cons(x1 + x2, addLists(xs1, xs2))
    }
  }

  def addLists2(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 + x2, addLists2(xs1, xs2))
  }

  /*
   3.23
   */
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
  }

  /*
   3.24
   */
  @tailrec
  def hasSubsequence[A](list: List[A], subseq: List[A]): Boolean = {

    @tailrec
    def startsWith(l: List[A], s: List[A]): Boolean = (l, s) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(x, xs), Cons(y, ys)) => x == y && startsWith(xs, ys)
    }

    list match {
      case Nil => subseq == Nil
      case Cons(x, xs) => startsWith(list, subseq) || hasSubsequence(xs, subseq)
    }
  }
}
