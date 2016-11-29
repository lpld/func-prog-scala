package com.github.lpld.study.funcscala.chapter6

import com.github.lpld.study.funcscala.chapter6.State.unit

/**
  * @author leopold
  * @since 29/11/16
  */
case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(run andThen { case (a, s) => f(a).run(s) })

  def map[B](f: A => B): State[S, B] = flatMap(f andThen unit)

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs match {
    case Nil => unit(Nil)
    case a :: as => a.map2(sequence(as))(_ :: _)
  }
}
