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
  //  def jsonParsers[Parser[+_]](P: Parsers[Parser]): JsonParsers[Parser] = JsonParsers(P)

  def j[Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    def brackets = (token('['), token(']'))

    def braces = (token('{'), token('}'))

    def comma = token(',')

    def colon = token(':')

    def stringExpr = token(text inside '"')

    def jNull = string("null").t map (_ => JNull)

    def jNumber = double.t map JNumber

    def jString = stringExpr.t map JString

    def jBool = ("true" | "false").t map (_.toBoolean) map JBool

    def field: Parser[(String, JSON)] = scope("Field") {
      (stringExpr as "Field name") *~ colon ** (json as "Field value")
    }

    def json: Parser[JSON] = scope("JSON value") {
      jNull | jNumber | jString | jBool | jArray | jObject
    }

    def jArray: Parser[JArray] = /*scope("Array") {*/
      (json by comma inside brackets) map (_.toIndexedSeq) map JArray

    //    }
    def jObject: Parser[JObject] = scope("Object") {
      (field by comma inside braces) map (_.toMap) map JObject
    }

    jArray | jObject
  }
}

