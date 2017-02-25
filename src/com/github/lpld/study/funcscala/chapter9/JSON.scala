package com.github.lpld.study.funcscala.chapter9

import scala.language.postfixOps

/**
  * @author leopold
  * @since 25/02/17
  */
trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

  /*
   * 9.9. Implement JSON parser
   */
  def jsonParser[Err, Parser[+ _]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._

    object Parser {

      val brackets = (token('['), token(']'))
      val braces = (token('{'), token('}'))
      val comma = token(',')
      val colon = token(':')

      val stringExpr = text \\> '"'

      val jNull = string("null").t map (_ => JNull)
      val jNumber = double.t map JNumber
      val jString = stringExpr map JString
      val jBool = ("true" | "false").t map (_.toBoolean) map JBool

      val field: Parser[(String, JSON)] = stringExpr *~ colon ** json

      val jArray: Parser[JArray] = (json *\ comma \\> brackets) map (_.toIndexedSeq) map JArray
      val jObject: Parser[JObject] = (field *\ comma \\> braces) map (_.toMap) map JObject

      def json: Parser[JSON] = jNull | jNumber | jString | jBool | jArray | jObject
    }

    Parser.json
  }

}

