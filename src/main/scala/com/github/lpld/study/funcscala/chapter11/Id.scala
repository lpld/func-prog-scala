package com.github.lpld.study.funcscala.chapter11

/**
  * @author leopold
  * @since 15/03/17
  */
/*
 * 10.17. Implement `map` and `flatMap` as methods on `Id` class, and
 * give an implementation for `Monad[Id]`.
 */
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma flatMap f
  }
}
