package com.github.lpld.study.funcscala.chapter12

import com.github.lpld.study.funcscala.chapter11.Functor

/**
  * @author leopold
  * @since 15/03/17
  */
trait Applicative[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]
  /*
   * 12.2. Define `map` and `map2` in terms of `unit` and `apply`. Implement
   * apply in terms of `map2` and `unit`.
   */
  def apply[A, B](fab: F[A => B])(f: F[A]): F[B] =
    map2(fab, f)(_ apply _)

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit[A => B](f))(fa)

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    apply(map(ma)(f.curried): F[B => C])(mb)

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, fla) => map2(f(a), fla)(_ :: _))

  /*
   * 12.1. Transplant the implementations of as many combinators as you can
   * from `Monad` to `Applicative`, using only `map2` and `unit`, or methods
   * implemented in terms of them.
   */
  def sequence[A](lma: List[F[A]]): F[List[A]] = traverse(lma)(identity)

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = map(ma)(a => List.fill(n)(a))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List.empty[A]))((a, acc) => map2(acc, f(a))((l, b) => if (b) a :: l else l))
}
