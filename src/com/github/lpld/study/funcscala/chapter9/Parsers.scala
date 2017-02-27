package com.github.lpld.study.funcscala.chapter9

import com.github.lpld.study.funcscala.chapter8.Gen.forAll
import com.github.lpld.study.funcscala.chapter8.{Gen, Prop}

import scala.language.implicitConversions
import scala.util.matching.Regex

/**
  * @author leopold
  * @since 23/02/17
  */
trait Parsers[Parser[+_]] { self =>

  // this implementation causes infinite recursion because
  // map is implemented in terms of succeed.
  def succeedViaMap[A](a: A): Parser[A] = string("") map (_ => a)

  def succeed[A](a: A): Parser[A]

  /*
   * 9.4. Using `map2` and `succeed`, implement the `listOfN` combinator.
   */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n < 0) throw new IllegalArgumentException
    else if (n == 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  val spaces: Parser[String] = "\\s*".r
  val digits: Parser[String] = "[+-]?(\\d*\\.)?\\d+".r
  val text: Parser[String] = "[^\\\"]*".r

  val double: Parser[Double] = digits map (_.toDouble)

  def token[A](p: Parser[A]): Parser[A] = spaces ** p map (_._2)

  /*
   * 9.7. Implement `product` in terms of `flatMap`
   */
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p1)(a => map(p2)((a, _)))

  /*
   * 9.8. Implement `map` in terms of `flatMap`
   */
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(f andThen succeed)

  def slice[A](p: Parser[A]): Parser[String]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  /*
   * 9.1 (pt. 1). Using product, implement map2
   */
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {a <- p1; b <- p2} yield f(a, b)
  // following does not always work. todo: why?
  //    product(p1, p2) map { case (a, b) => f(a, b) }

  implicit def string(s: String): Parser[String]

  implicit def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def asCharParser[A](a: A)(implicit f: A => Parser[Char]): ParserOps[Char] = ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    /*
     * 9.1 (pt. 2). Use map2 to implement many1
     */
    def many1: Parser[List[A]] = map2(p, p.many)(_ :: _)

    /*
     * 9.3. Define `many` in terms of `or`, `map2` and `succeed`
     */
    def many: Parser[List[A]] = map2(p, p.many)(_ :: _) | succeed(List())

    def slice: Parser[String] = self.slice(p)

    def ~:(left: Parser[_]): Parser[A] = left ** p map (_._2)

    def :~(right: Parser[_]): Parser[A] = p ** right map (_._1)

    // parse but ignore
    def *~ = :~ _

    def ~* = ~: _

    // many, separated by `s`
    def by(s: Parser[_]): Parser[List[A]] =
      map2(p *~ s, p by s)(_ :: _) | (p map (List(_))) | succeed(Nil)

    def enclosed(l: Parser[_], r: Parser[_]): Parser[A] = l ~: p :~ r

    // operator `enclosed in`. means that p is expected to be enclosed between two values `a`
    def inside(a: Parser[_]): Parser[A] = enclosed(a, a)

    def inside(p: (Parser[_], Parser[_])): Parser[A] = enclosed(p._1, p._2)

    // returns parser for token, i.e that allows trailing whitespaces
    def t: Parser[A] = token(p)

    def as(msg: String): Parser[A] = label(msg)(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p map identity)(in)

    def unitLaw[A](as: Gen[A], ss: Gen[String]): Prop =
      forAll(as.map2(ss)((_, _))) { case (a, s) => run(succeed(a))(s) == Right(a) }

    /*
     * 9.2. Try coming up with laws to specify the behavior of `product`
     */
    def assocProduct[A, B, C](p1: Parser[A], p2: Parser[B], p3: Parser[C])(in: Gen[String]): Prop =
      forAll(in) { s =>
        val r1: Either[ParseError, ((A, B), C)] = run((p1 ** p2) ** p3)(s)
        val r2: Either[ParseError, (A, (B, C))] = run(p1 ** (p2 ** p3))(s)

        val e1 = r1.right.map(p => (p._1._1, p._1._2, p._2))
        val e2 = r2.right.map(p => (p._1, p._2._1, p._2._2))

        e1 == e2
      }

    def mapProduct[A, B, C, D](p1: Parser[A], p2: Parser[B])(f: A => C, g: B => D)(in: Gen[String]): Prop =
      equal(
        p1.map(f) ** p2.map(g),
        p1 ** p2 map { case (a, b) => (f(a), g(b)) }
      )(in)
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val column = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }
}

case class ParseError(stack: List[(Location, String)])
