package com.github.lpld.study.funcscala.chapter4

/**
  * @author leopold
  * @since 26/08/16
  */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = flatMap(value => if (f(value)) Some(value) else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


