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
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    object Parser {

      val brackets = (token('['), token(']'))
      val braces = (token('{'), token('}'))
      val comma = token(',')
      val colon = token(':')

      val stringExpr = text inside '"'

      val jNull = string("null").t map (_ => JNull)
      val jNumber = double.t map JNumber
      val jString = stringExpr.t map JString
      val jBool = ("true" | "false").t map (_.toBoolean) map JBool

      val field: Parser[(String, JSON)] = scope("Field") {
        (stringExpr as "Field name") *~ colon ** (json as "Field value")
      }

      val jArray: Parser[JArray] = scope("Array") {
        (json by comma inside brackets) map (_.toIndexedSeq) map JArray
      }
      val jObject: Parser[JObject] = scope("Object") {
        (field by comma inside braces) map (_.toMap) map JObject
      }

      def json: Parser[JSON] = scope("JSON value") {
        jNull | jNumber | jString | jBool | jArray | jObject
      }
    }

    Parser.json
  }

}

