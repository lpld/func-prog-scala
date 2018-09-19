package com.github.lpld.study.funcscala.chapter13.io

/**
  * @author leopold
  * @since 18/09/18
  */
sealed trait IO[A] {

  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
}

case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

object IO {

  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    //    case FlatMap(x, f) => run(f(run(x)))
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      //      case FlatMap(y, g) =>
      //        run((y flatMap g) flatMap f)
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }

  def forever[A, B](a: IO[A]): IO[B] = {
    lazy val t: IO[B] = forever(a)
    a flatMap (_ => t)
    // FlatMap(a, _ => forvever(a))
    // FlatMap(a, _ => FlatMap(a, _ => FlatMap(a, _ => FlatMap(a, ...))))
    // where a is Suspend(...)
  }
}

object Run extends App {

  val p: IO[Unit] = Suspend(() => println("abc"))
  val print: IO[Unit] = (1 to 2).foldLeft(p) { (pp, _) => pp.flatMap(x => Return(x)) }

  //  val foreverPrint = IO.forever(p)
  //
  val a: Unit = IO.run(print)
}
