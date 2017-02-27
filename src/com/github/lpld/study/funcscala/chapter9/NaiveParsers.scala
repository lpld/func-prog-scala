package com.github.lpld.study.funcscala.chapter9

import com.github.lpld.study.funcscala.chapter9.NaiveParsers.{Parts, Res, simpleError}

import scala.language.implicitConversions
import scala.util.matching.Regex

/**
  * @author leopold
  * @since 26/02/17
  */
class NaiveParsers extends Parsers[NaiveParser] {

  override def succeed[A](a: A): NaiveParser[A] = UnitParser(a)

  override def or[A](p1: NaiveParser[A], p2: => NaiveParser[A]): NaiveParser[A] = new AlternativeParser(p1, p2)

  override def run[A](p: NaiveParser[A])(input: String): Either[ParseError, A] =
    (p parse input).right map (_._1)

  override def slice[A](p: NaiveParser[A]): NaiveParser[String] = Slicer(p)

  override def flatMap[A, B](p: NaiveParser[A])(f: (A) => NaiveParser[B]): NaiveParser[B] = SequentialParser(p, f)

  override def label[A](msg: String)(p: NaiveParser[A]): NaiveParser[A] = p

  override def scope[A](msg: String)(p: NaiveParser[A]): NaiveParser[A] = p

  override def attempt[A](p: NaiveParser[A]): NaiveParser[A] = p

  override implicit def string(s: String): NaiveParser[String] = StringParser(s)

  override implicit def regex(r: Regex): NaiveParser[String] = RegexParser(r)
}

object NaiveParsers {
  type Parts = (String, String)
  type Res[+T] = (T, Parts)

  def simpleError(input: String) = Left(ParseError(List(Location(input) -> "Error")))
}

trait NaiveParser[+T] {
  def matchInput(in: String): Option[Parts]

  def parse(in: String): Either[ParseError, Res[T]]
}

trait ToStringParser extends NaiveParser[String] {
  override def parse(in: String): Either[ParseError, Res[String]] =
    matchInput(in) map { case (m, r) => Right(m, (m, r)) } getOrElse simpleError(in)
}

case class UnitParser[T](v: T) extends NaiveParser[T] {
  override def matchInput(in: String): Option[(String, String)] = Some(("", in))

  override def parse(in: String): Either[ParseError, (T, (String, String))] = Right((v, ("", in)))
}

case class Slicer(p: NaiveParser[_]) extends ToStringParser {
  override def matchInput(in: String): Option[Parts] = p matchInput in
}

case class StringParser(str: String) extends ToStringParser {
  override def matchInput(in: String): Option[Parts] =
    if (in.startsWith(str)) Some((str, in.substring(str.length)))
    else None
}

case class RegexParser(r: Regex) extends ToStringParser {
  override def matchInput(in: String): Option[Parts] =
    r findPrefixOf in map { s => (s, in.substring(s.length)) }
}

class AlternativeParser[T](p1: NaiveParser[T], p2: => NaiveParser[T]) extends NaiveParser[T] {
  override def matchInput(in: String): Option[Parts] =
    (p1 matchInput in) orElse (p2 matchInput in)

  override def parse(in: String): Either[ParseError, Res[T]] =
    p1 parse in match {
      case Left(_) => p2 parse in
      case success => success
    }
}

case class SequentialParser[A, B](p: NaiveParser[A], f: A => NaiveParser[B]) extends NaiveParser[B] {
  override def matchInput(in: String): Option[Parts] =
    p parse in match {
      case Right((a, (m, rest))) => (f(a) matchInput rest) map { case (m2, rest2) => (m + m2, rest2) }
      case _ => None
    }

  override def parse(in: String): Either[ParseError, Res[B]] =
    for {
      r <- (p parse in).right
      r2 <- (f(r._1) parse r._2._2).right
    } yield (r2._1, (r._2._1 + r2._2._1, r2._2._2))

}