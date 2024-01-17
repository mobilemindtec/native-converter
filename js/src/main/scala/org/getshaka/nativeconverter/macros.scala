package org.getshaka.nativeconverter

import org.getshaka.nativeconverter.JsonType.JsonDate

import scala.quoted.*
import scala.annotation.{MacroAnnotation, StaticAnnotation}
import scala.scalajs.js
import scala.scalajs.js.Date


case class JsonField(fieldName: String,
                     jsonName: Option[String] = None,
                     ignore: Boolean = false,
                     omitNull: Boolean = false,
                     typ: JsonType = JsonType.JsonAuto)

object JsonField:
  def apply(fieldName: String, json: Json): JsonField =
    val name = Some(json.name).filter(_.nonEmpty)
    new JsonField(fieldName, name, json.ignore, json.omitNull, json.typ)



case class JsonFields[T](values: List[JsonField]):
  def findField(key: String): Option[JsonField] =
    this.values
      .find(_.fieldName == key)
      //.filterNot(_.ignore)

  def findJsonName(key: String): Option[String] =
    findField(key) match
      case Some(field) =>
        if field.ignore
        then None
        else Some(field.jsonName.getOrElse(key))
      case _ => Some(key)

  // scala to js
  def toNative(key: String, v: Any): js.Any =
    findField(key)
      .map { fld =>
        fld.typ match
          case JsonType.JsonIntStr =>
            v match
              case _: Int => v.toString
              case _ => 0
          case JsonType.JsonNumStr =>
            v match
              case _: Double => v.toString
              case _ => 0.0
          case d@JsonType.JsonDate(pattern) =>
            v match
              case s: Date =>
                  d.dateConverter.format(s, pattern)
              case _ => null
          case JsonType.JsonBoolStr =>
            v match
              case b: Boolean => b.toString
              case _ => "false"
          case JsonType.JsonAuto => v
      }
      .map(_.asInstanceOf[js.Any])
      .getOrElse(v.asInstanceOf[js.Any])

  // js to scala
  def fromNative(key: String, v: Any): js.Any =
    findField(key)
      .map { fld =>

        fld.typ match
          case JsonType.JsonIntStr =>
            v match
              case _: Int => v
              case s: String => s.toIntOption.getOrElse(0)
              case d: Double => d.toInt
              case _ => 0
          case JsonType.JsonNumStr =>
            v match
              case _: Double => v
              case s: String => s.toDoubleOption.getOrElse(0)
              case d: Int => d.toDouble
              case _ => 0.0
          case d@JsonType.JsonDate(pattern) =>
            v match
              case s: String =>
                d.dateConverter.parse(s, pattern)
              case _ => null
          case JsonType.JsonBoolStr =>
            v match
              case s: String => s == "true"
              case i: Int => i == 1
              case _ => false
          case JsonType.JsonAuto => v
      }
      .map(_.asInstanceOf[js.Any])
      .getOrElse(v.asInstanceOf[js.Any])

object JsonFields:
  def apply[T](values: Vector[(String, Any)]): JsonFields[T] =
    new JsonFields(values.map {
      case (k, v: Json) => JsonField(k, v)
    }.toList)

inline def getAnnotations[T]: JsonFields[T] = ${ getAnnotationsImpl[T] }

def getAnnotationsImpl[T: Type](using q: Quotes): Expr[JsonFields[T]] =
  import quotes.reflect.*
  val annotJson = TypeRepr.of[Json].typeSymbol
  val tuples: Seq[Expr[(String, Any)]] = TypeRepr
    .of[T]
    .typeSymbol
    .primaryConstructor
    .paramSymss
    .flatten
    .collect {
      case sym if sym.hasAnnotation(annotJson) =>
        val fieldNameExpr = Expr(sym.name.asInstanceOf[String])
        val annotExpr = sym.getAnnotation(annotJson).get.asExprOf[Json]
        '{ ($fieldNameExpr, $annotExpr) }
    }
  val seq: Expr[Seq[(String, Any)]] = Expr.ofSeq(tuples)
  '{ JsonFields[T]($seq.toVector) }