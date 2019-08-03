package com.github.lpld.study.funcscala.chapter13

import com.github.lpld.study.funcscala.chapter7.Par
import com.github.lpld.study.funcscala.chapter7.Par.Par

import scala.annotation.tailrec

/**
  * @author leopold
  * @since 2019-08-03
  */
sealed trait Async[A] {

  def flatMap[B](f: A ⇒ Async[B]): Async[B] = Async.FlatMap(this, f)
  def map[B](f: A ⇒ B): Async[B] = flatMap(f andThen (Async.Return(_)))
}


object Async {


  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A]
  case class FlatMap[A, B](sub: Async[A], k: A ⇒ Async[B]) extends Async[B]

  @tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x, f), g) ⇒ step(x flatMap (a ⇒ f(a) flatMap g))
    case FlatMap(Return(x), f)     ⇒ step(f(x))
    case _                         ⇒ async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a)     ⇒ Par.unit(a)
    case Suspend(r)    ⇒ r // ???. In book 
    case FlatMap(x, f) ⇒ x match {
      case Suspend(r) ⇒ Par.flatMap(r)(a ⇒ run(f(a)))
      case _          ⇒ sys.error("Impossible; `step` eliminates these cases")
    }
  }

}
