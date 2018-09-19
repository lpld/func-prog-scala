package com.github.lpld.study.funcscala.chapter2

import scala.annotation.tailrec

/**
  * @author leopold
  * @since 4/06/16
  */
object Chapter2 {

  /*
     2.1 Fibonacci
   */
  def fib(n: Int): Int = {

    @tailrec
    def loop(lastButOne: Int, last: Int, n: Int): Int =
      if (n == 0) lastButOne
      else loop(last, last + lastButOne, n - 1)

    loop(1, 1, n)
  }

  def fibonacciString(n: Int) = 0 to n map fib mkString " "

  /*
   2.2 Is Sorted
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean) = {
    val ass = as.toStream
    def pairOrdered(t: (A, A)) = ordered(t._1, t._2)

    ass zip ass.tail forall pairOrdered
  }

  /*
   2.3 Currying
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  /*
   2.4 Uncurrying
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  /*
   2.5 Composition
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
