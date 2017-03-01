package com.github.lpld.study.funcscala.chapter9

import scala.language.postfixOps
import language.higherKinds
import language.implicitConversions

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
  def jsonParser[Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    val quotedString = token(text inside '"')

    val jNull = "null" as JNull
    val jNumber = token(double) map JNumber
    val jString = quotedString map JString
    val jBool = ("true" as JBool(true)) | ("false" as JBool(false))

    def jArray = scope("Array")(
      value by "," inside("[", "]")
        map (l => JArray(l.toIndexedSeq))
    )

    def jObject = scope("Object")(
      (quotedString *~ ":" ** value) by "," inside("{", "}")
        map (v => JObject(v.toMap))
    )

    def value: Parser[JSON] = scope("JSON value") {
      jNull | jNumber | jString | jBool | jArray | jObject
    }

    jArray | jObject
  }
}

