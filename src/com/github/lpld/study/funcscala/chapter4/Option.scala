package com.github.lpld.study.funcscala.chapter4

/**
  * @author leopold
  * @since 26/08/16
  */
sealed trait Option[+A] {

  /*
   4.1
   */
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

object Option {
  def Try[A](a: => A) = try Some(a)
  catch {
    case e: Exception => None
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case opt :: xs => opt flatMap (x => sequence(xs) map (x :: _))
    //    case Some(x) :: xs => sequence(xs) map (x :: _)
    //    case None :: _ => None
    case Nil => Some(Nil)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case x :: xs => for (v <- f(x); vs <- traverse(xs)(f)) yield v :: vs
    //      f(x) flatMap (v => traverse(xs)(f) map (v :: _))
    case Nil => Some(Nil)
  }

  def sequenceViaTraverse[A](a: List[Option[A]]) = traverse(a)(identity)
}
