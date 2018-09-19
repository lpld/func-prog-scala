package com.github.lpld.study.funcscala.chapter13

import com.github.lpld.study.funcscala.chapter11.Monad
import com.github.lpld.study.funcscala.chapter13.Translate.~>
import com.github.lpld.study.funcscala.chapter7.Par
import com.github.lpld.study.funcscala.chapter7.Par.Par

import scala.io.StdIn

/**
  * @author leopold
  * @since 19/09/18
  */
sealed trait Console[A] {

  def toPar: Par[A]
  def toThunk: () => A
}

case object ReadLine extends Console[Option[String]] {

  def toPar = Par.lazyUnit(run)
  def toThunk = () => run

  def run: Option[String] =
    try Some(StdIn.readLine())
    catch { case _: Exception => None }
}

case class PrintLine(line: String) extends Console[Unit] {

  def toPar = Par.lazyUnit(println(line))
  def toThunk = () => println(line)
}

object Console {

  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

  val consoleToFunction0: Console ~> Function0 =
    new (Console ~> Function0) {def apply[A](c: Console[A]): () => A = c.toThunk }

  val consoleToPar: Console ~> Par =
    new (Console ~> Par) {def apply[A](c: Console[A]): Par[A] = c.toPar }

  implicit val function0Monad = new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a
    override def flatMap[A, B](ma: () => A)(f: A => () => B): () => B = () => f(ma())()
  }

  implicit val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.fork { Par.flatMap(ma)(f) }
  }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A =
    Free.runFree[Console, Function0, A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console, A]): Par[A] =
    Free.runFree[Console, Par, A](a)(consoleToPar)
}
