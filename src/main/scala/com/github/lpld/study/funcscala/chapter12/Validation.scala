package com.github.lpld.study.funcscala.chapter12

import scala.language.reflectiveCalls

/**
  * @author leopold
  * @since 15/03/17
  */
sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {
  /*
   * 12.6. Write an `Applicative` instance for `Validation`.
   */
  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    def unit[A](a: => A): Validation[E, A] = Success(a)

    override def apply[A, B](fab: Validation[E, (A) => B])(v: Validation[E, A]): Validation[E, B] =
      (v, fab) match {
        case (Success(a), Success(f)) => Success(f(a))
        case (Success(_), Failure(e, es)) => Failure(e, es)
        case (Failure(e, es), Success(_)) => Failure(e, es)
        case (Failure(e1, es1), Failure(e2, es2)) => Failure(e1, (es1 :+ e2) ++ es2)
      }
  }
}
