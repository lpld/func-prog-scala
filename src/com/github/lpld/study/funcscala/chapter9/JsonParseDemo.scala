package com.github.lpld.study.funcscala.chapter9

/**
  * @author leopold
  * @since 26/02/17
  */
object JsonParseDemo extends App {
  val parsers = new NaiveParsers

  val jsonParser = JSON.jsonParser(parsers)

  val jsonString =
    """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""

  println(parsers.run(jsonParser)(jsonString))


}
