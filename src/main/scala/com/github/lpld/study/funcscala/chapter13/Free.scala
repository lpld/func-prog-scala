package com.github.lpld.study.funcscala.chapter13

import com.github.lpld.study.funcscala.chapter11.Monad
import com.github.lpld.study.funcscala.chapter13.Translate.~>
import com.github.lpld.study.funcscala.chapter7.Par.Par

import scala.annotation.tailrec
import scala.language.{higherKinds, reflectiveCalls}

/**
  * @author leopold
  * @since 18/09/18
  */
sealed trait Free[F[_], A] {

  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))
}

case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](r: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  /**
    * 13.1. Implement `map` and `flatMap` methods on the `Free` trait and give the Monad instance
    * for `Free[F, _]`
    */
  def freeMonad[F[_]] = new Monad[({type f[a] = Free[F, a]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)
    override def flatMap[A, B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = ma.flatMap(f)
    override def map[A, B](ma: Free[F, A])(f: A => B): Free[F, B] = ma.map(f)
  }

  /**
    * 13.2. Implement a specialized tail recursive interpreter, `runTrampoline`, for
    * running `Free[Function0, A]`
    */
  def runTrampoline[A](tr: TailRec[A]): A = tr match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(s, f) => s match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(s1, f1) => runTrampoline(s1 flatMap (a => f1(a) flatMap f))
    }
  }

  @tailrec def step[F[_], A](fr: Free[F, A]): Free[F, A] = fr match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => fr
  }

  /**
    * 13.3. Implement a generic interpreter for `Free[F, A]` given a `Monad[F]`.
    */
  def run[F[_], A](free: Free[F, A])(implicit M: Monad[F]): F[A] = step(free) match {
    case Return(a) => M.unit(a)
    case Suspend(r) => r
    case FlatMap(Suspend(r), f) => M.flatMap(r)(a => run(f(a)))
    case _ => sys.error("")
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("")
    }
}
