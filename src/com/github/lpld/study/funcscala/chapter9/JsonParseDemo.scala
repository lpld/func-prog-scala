package com.github.lpld.study.funcscala.chapter9

import com.github.lpld.study.funcscala.chapter9.JSON.JArray

/**
  * @author leopold
  * @since 26/02/17
  */
object JsonParseDemo extends App {
  val parsers = new NaiveParsers

  import parsers._

  val p = JSON.jsonParser(parsers)

//  import jsonParsers._

  val jsonString =
    """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""

      println(parsers.run(p)(jsonString))

//      println(parsers.run(p)("[]"))
//  println(parsers.run((json by comma inside brackets) map (_.toIndexedSeq) map JArray)("[]"))
//  println(parsers.run(jArray)("[]"))

//    println(parsers.run(p)("\"abc sdf\"   :   [5 , 8] "))

  //    println(parsers.run(stringExpr *~ ':'.t ** jArray)("\"abc\":[ ]"))

  //  println(parsers.run('[' ** '{')("[{"))
}
