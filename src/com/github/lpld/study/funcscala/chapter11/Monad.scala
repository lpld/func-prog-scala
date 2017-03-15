package com.github.lpld.study.funcscala.chapter11

import com.github.lpld.study.funcscala.chapter6.State
import com.github.lpld.study.funcscala.chapter7.Par
import com.github.lpld.study.funcscala.chapter7.Par.Par
import com.github.lpld.study.funcscala.chapter8.Gen
import com.github.lpld.study.funcscala.chapter9.Parsers

import scala.language.reflectiveCalls

/**
  * @author leopold
  * @since 14/03/17
  */
trait Monad[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  /*
   * 11.3. Implement `sequence` and `traverse` combinators.
   */
  def sequence[A](lma: List[F[A]]): F[List[A]] = traverse(lma)(identity)

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, fla) => map2(f(a), fla)(_ :: _))

  /*
   * 11.4. Implement `replicateM`
   */
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = map(ma)(a => List.fill(n)(a))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  /*
   * 11.6. Implement `filterM` function.
   */
  def filterMViaFoldRight[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List.empty[A]))((a, acc) => map2(acc, f(a))((l, b) => if (b) a :: l else l))

  def filterMViaTraverse[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    map(traverse(ms)(a => map(f(a))(b => (a, b))))(_.filter(_._2).map(_._1))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case a :: as => map2(f(a), filterM(as)(f))((b, l) => if (b) a :: l else l)
    }

  /*
   * 11.7. Implement the Kleisli composition function `compose`.
   */
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  /*
   * 11.8. Implement `flatMap` in terms of `compose`.
   */
  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

  /*
   * 11.9. Implement `join` in terms of `flatMap`.
   */
  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(identity)

  /*
   * 11.10. Implement either `flatMap` or `compose` in terms of `join` and `map`.
   */
  def flatMapViaJoin[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def composeViaJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))
}

object Monad {
  val genMonad = new Monad[Gen]  {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](ma: Gen[A])(f: (A) => Gen[B]): Gen[B] = ma flatMap f
  }

  /*
   * 11.1. Write `Monad` instances for `Par`, `Parser`, `Option`, `Stream` and `List`.
   */
  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
    def unit[A](a: => A): P[A] = p.succeed(a)
    def flatMap[A, B](ma: P[A])(f: (A) => P[B]): P[B] = p.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma flatMap f
  }

  /*
   * 11.2. Implement a `State` monad.
   */
  def stateMonad[S] = new Monad[({type f[+A] = State[S, A]})#f] {
      def unit[A](a: => A): State[S, A] = State.unit(a)
      def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ma.flatMap(f)
  }

}
