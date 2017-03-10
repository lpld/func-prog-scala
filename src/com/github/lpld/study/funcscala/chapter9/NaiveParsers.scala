package com.github.lpld.study.funcscala.chapter9

import com.github.lpld.study.funcscala.chapter9.NaiveParsers._

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
    p.parseRoot(input).right map (_._1)

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
  type RootOffset = (String, Int)

  def matched(p: Parts) = p._1

  def rest(p: Parts) = p._2

  def root(rootOffset: RootOffset) = rootOffset._1

  def offset(rootOffset: RootOffset) = rootOffset._2

  def simpleError(input: String) = Left(ParseError(List(Location(input) -> "Error")))

  def locationError(rootOffset: RootOffset) =
    Left(ParseError(List(Location(root(rootOffset), offset(rootOffset)) -> "Bad input")))
}

trait NaiveParser[+T] {
  def matchInput(in: String, rootOffset: RootOffset): Option[Parts]

  def parse(in: String, rootOffset: RootOffset): Either[ParseError, Res[T]]

  def parseRoot(in: String): Either[ParseError, Res[T]] = parse(in, (in, 0))
}

trait ToStringParser extends NaiveParser[String] {
  override def parse(in: String, rootOffset: RootOffset): Either[ParseError, Res[String]] =
    matchInput(in, rootOffset) map { case (m, r) => Right(m, (m, r)) } getOrElse locationError(rootOffset)
}

case class UnitParser[T](v: T) extends NaiveParser[T] {
  override def matchInput(in: String, rootOffset: RootOffset): Option[(String, String)] = Some(("", in))

  override def parse(in: String, rootOffset: RootOffset): Either[ParseError, (T, (String, String))] = Right((v, ("", in)))
}

case class Slicer(p: NaiveParser[_]) extends ToStringParser {
  override def matchInput(in: String, rootOffset: RootOffset): Option[Parts] = p.matchInput(in, rootOffset)
}

case class StringParser(str: String) extends ToStringParser {
  override def matchInput(in: String, rootOffset: RootOffset): Option[Parts] =
    if (in.startsWith(str)) Some((str, in.substring(str.length)))
    else None
}

case class RegexParser(r: Regex) extends ToStringParser {
  override def matchInput(in: String, rootOffset: RootOffset): Option[Parts] =
    r findPrefixOf in map { s => (s, in.substring(s.length)) }
}

class AlternativeParser[T](p1: NaiveParser[T], p2: => NaiveParser[T]) extends NaiveParser[T] {
  override def matchInput(in: String, rootOffset: RootOffset): Option[Parts] =
    p1.matchInput(in, rootOffset) orElse p2.matchInput(in, rootOffset)

  override def parse(in: String, rootOffset: RootOffset): Either[ParseError, Res[T]] =
    p1.parse(in, rootOffset) match {
      case Left(_) => p2.parse(in, rootOffset)
      case success => success
    }
}

case class SequentialParser[A, B](p: NaiveParser[A], f: A => NaiveParser[B]) extends NaiveParser[B] {
  override def matchInput(in: String, rootOffset: RootOffset): Option[Parts] = {

    p.parse(in, rootOffset) match {
      case Right((a, (m, rest))) =>
        f(a).matchInput(rest, newOffset(rootOffset, m)) map { case (m2, rest2) => (m + m2, rest2) }
      case _ => None
    }
  }

  private def newOffset(rootOffset: RootOffset, matched: String) =
    (root(rootOffset), offset(rootOffset) + matched.length)

  override def parse(in: String, rootOffset: RootOffset): Either[ParseError, Res[B]] =
    for {
      r <- p.parse(in, rootOffset).right
      r2 <- f(r._1).parse(r._2._2, newOffset(rootOffset, r._2._1)).right
    } yield (r2._1, (r._2._1 + r2._2._1, r2._2._2))

}