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

  def flatMap[B](f: A => Free[F, B]): Free[F, B] = Free.FlatMap(this, f)
  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Free.Return(_)))
}


object Free {

  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](r: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  /** 13.1. Give the monad instance for Free[F, _]. */
  def freeMonad[F[_]] = new Monad[({type f[a] = Free[F, a]})#f] {

    def unit[A](a: ⇒ A): Free[F, A] = Return(a)
    override def flatMap[A, B](ma: Free[F, A])(f: A ⇒ Free[F, B]): Free[F, B] = FlatMap(ma, f)
  }

  /** 13.2. Implement a specialized tail-recursive interpreter, `runTrampoline`, for running
    * Free[Function0, A]. */
  @tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a)     ⇒ a
    case Suspend(a)    ⇒ a()
    case FlatMap(b, f) ⇒ b match {
      case Return(x)       ⇒ runTrampoline(f(x))
      case Suspend(xx)     ⇒ runTrampoline(f(xx()))
      case FlatMap(xx, ff) ⇒ runTrampoline(xx flatMap (a ⇒ ff(a) flatMap f))
    }
  }

  @tailrec
  def step[F[_], B](fr: Free[F, B]): Free[F, B] = fr match {
    case FlatMap(Return(b), f)     ⇒ step(f(b))
    case FlatMap(FlatMap(b, f), g) ⇒ step(b flatMap (a ⇒ f(a) flatMap g))
    case _                         ⇒ fr
  }

  /** 13.3. Implement `run` for `Free[F, A]`. */
  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = {

    step(a) match {
      case Return(a)      ⇒ F.unit(a)
      case Suspend(r)     ⇒ r
      case FlatMap(fr, f) ⇒ fr match {
        case Suspend(r) ⇒ F.flatMap(r)(a ⇒ run(f(a)))
        case _          ⇒ sys.error("Impossible; `step` eliminates these cases")
      }
    }
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("")
    }

  /** 13.4. (part 1). */
  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[x] = Free[G, x]
    val ffg: F ~> FreeG = new (F ~> FreeG) {
      def apply[x](f: F[x]): FreeG[x] = Suspend(fg.apply(f))
    }

    runFree(f)(ffg)(freeMonad[G])
  }
}
