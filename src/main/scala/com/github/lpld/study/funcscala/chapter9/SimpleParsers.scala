package com.github.lpld.study.funcscala.chapter9

/**
  * @author leopold
  * @since 26/02/17
  */
class SimpleParsers[Parser[+_]](P: Parsers[Parser]) {

  import P._
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
      str <- listOfN(n, char(c)).slice
    } yield str.length
}

object ParsersDemo extends App {
  private val parsers = new NaiveParsers
  private val ps = new SimpleParsers(parsers)
  import parsers._

  private val seqOfA = ps.sequenceOfFor('a')

  println(parsers.run(seqOfA)("4aaaa"))

//  println(run(string("abc").slice)("abcd"))

//  println(run(("\\w".r flatMap(s => char(s.charAt(0)))).slice)("aab"))

  // todo: there's an issue with slice after map2
  //  println(run(map2("\\d".r , "\\d".r)(_.toDouble + _.toDouble).slice)("123"))

//  println(run((string("vb") flatMap (_ => succeed('t'))).slice)("vbn"))

//    println(run(parsers.listOfN(1, 'c').slice)("cccca"))

//  println(parsers.run(parsers.double)("5.53ghjg"))

//  println(parsers.run(parsers.succeed(5.52))("abcdef"))
}
