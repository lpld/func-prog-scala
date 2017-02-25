package com.github.lpld.study.funcscala.chapter9

import com.github.lpld.study.funcscala.chapter8.Gen.forAll
import com.github.lpld.study.funcscala.chapter8.{Gen, Prop}

import scala.language.implicitConversions
import scala.util.matching.Regex

/**
  * @author leopold
  * @since 23/02/17
  */
trait Parsers[ParseError, Parser[+_]] { self =>

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def orString(s1: String, s2: String): Parser[String]

  /*
   * 9.4. Using `map2` and `succeed`, implement the `listOfN` combinator.
   */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n < 0) throw new IllegalArgumentException
    else if (n == 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  def slice[A](p: Parser[A]): Parser[String]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  /*
   * 9.1 (pt. 1). Using product, implement map2
   */
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    product(p1, p2) map { case (a, b) => f(a, b) }

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

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

  object Methods {

    /*
     * 9.6 Context sensitive parser, that handles strings like
     * 0
     * 1a
     * 2aa
     * 4aaaa
     * and returns number of chars
     */
    def sequenceOf(c: Char): Parser[Int] =
      "\\d+".r map (_.toInt) flatMap (listOfN(_, char(c)).slice map (_.length))
    

    def sequenceOfFor(c: Char): Parser[Int] = for {
      digits <- "\\d+".r
      n = digits.toInt
      str <- listOfN(n, char('c')).slice
    } yield str.length
  }
}
