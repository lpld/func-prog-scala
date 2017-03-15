package com.github.lpld.study.funcscala.chapter11

import scala.language.reflectiveCalls

/**
  * @author leopold
  * @since 15/03/17
  */
case class Reader[R, A](run: R => A)

/*
 * 11.20. Give a monad instance for `Reader` type.
 */
object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    override def flatMap[A, B](ma: Reader[R, A])(f: (A) => Reader[R, B]): Reader[R, B] =
      Reader(r => f(ma.run(r)).run(r) )
  }
}
