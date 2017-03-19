package com.github.lpld.study.funcscala.chapter12

import com.github.lpld.study.funcscala.chapter11.Functor

import scala.language.reflectiveCalls

/**
  * @author leopold
  * @since 15/03/17
  */
trait Applicative[F[_]] extends Functor[F] { self =>

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
   * 12.3. Implement `map3` and `map4`.
   */
  def map3[A, B, C, D](fa: F[A],
                       fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(map2(fa, fb)((a, b) => f(a, b, _)): F[C => D])(fc)

  def map4[A, B, C, D, E](fa: F[A],
                          fb: F[B],
                          fc: F[C],
                          fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(map3(fa, fb, fc)((a, b, c) => f(a, b, c, _)): F[D => E])(fd)

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

  /*
   * 12.9. Implement `product` function.
   */
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A): (F[A], G[A]) =
        (self.unit(a), G.unit(a))

      override def apply[A, B](fab: (F[(A) => B], G[(A) => B]))(f: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fab._1)(f._1), G.apply(fab._2)(f._2))
    }

  /*
   * 12.10. Implement `compose` function.
   */
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A): F[G[A]] =
        self.unit(G.unit(a))

      override def apply[A, B](fab: F[G[(A) => B]])(f: F[G[A]]): F[G[B]] =
        self.map2(fab, f)((gab, ga) => G.apply(gab)(ga))
    }


  /*
   * 12.12. Implement `sequence` over a `Map`.
   */
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map.empty[K, V])) { case ((k, fv), fm) => map2(fv, fm)((v, map) => map + (k -> v)) }
}
