package com.github.lpld.study.funcscala.chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}


/**
  * @author leopold
  * @since 4/12/16
  */
object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call: A = a(es).get
  })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  /*
   * 7.1
   */
  def map2_old[A, B, C](par1: Par[A], par2: Par[B])(f: (A, B) => C): Par[C] = es => {
    val r1 = par1(es)
    val r2 = par2(es)
    UnitFuture(f(r1.get, r2.get))
  }

  /*
   * 7.3
   */
  def map2[A, B, C](par1: Par[A], par2: Par[B])(f: (A, B) => C): Par[C] = es =>
    CompositeFuture(par1(es), par2(es), f)

  /*
   * 7.4
   */
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map2(parList, unit(()))((p, _) => p.sorted)

  def map[A, B](par: Par[A])(f: A => B): Par[B] = map2(par, unit(()))((p, _) => f(p))

  def sortPar2(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /*
   * 7.5
   */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))

  /*
   * 7.6
   */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {

    val options: List[Par[Option[A]]] = as.map(asyncF(Option(_).filter(f)))
    val parOptions: Par[List[Option[A]]] = sequence(options)
    map(parOptions)(_.flatten)
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isCancelled: Boolean = false

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = true
  }

  private case class CompositeFuture[A, B, C](f1: Future[A], f2: Future[B], combinator: (A, B) => C) extends Future[C] {
    override def isCancelled: Boolean = f1.isCancelled || f2.isCancelled

    override def get(): C = combinator(f1.get, f2.get)

    override def get(timeout: Long, unit: TimeUnit): C = {
      val start = System.currentTimeMillis
      val r1 = f1.get(timeout, unit)
      val duration = System.currentTimeMillis - start

      // second future is left with originalTimeout - time of 1st future execution
      val newTimeout = unit.toMillis(timeout) - duration
      val r2 = f2.get(newTimeout, TimeUnit.MILLISECONDS)

      combinator(r1, r2)
    }

    override def cancel(mayInterruptIfRunning: Boolean): Boolean =
    // intentionally using bitwise & instead of logical && to make sure
    // that both futures are cancelled
      f1.cancel(mayInterruptIfRunning) & f2.cancel(mayInterruptIfRunning)

    override def isDone: Boolean = f1.isDone && f2.isDone
  }

}