package com.github.lpld.study.funcscala.chapter4

/**
  * @author leopold
  * @since 22/10/16
  */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(a) => Left(a)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(r) => f(r)
    case Left(l) => Left(l)
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]


