package org.getshaka.nativeconverter

import scala.annotation.StaticAnnotation
import scala.scalajs.js

enum JsonType:
  case JsonIntStr
  case JsonNumStr
  case JsonBoolStr
  case JsonDate(pattern: String)(using val dateConverter: JsonDateConverter)
  case JsonAuto

case class Json(name: String = "",
                ignore: Boolean = false,
                omitNull: Boolean = false,
                // force type conversion
                typ: JsonType = JsonType.JsonAuto) extends StaticAnnotation

trait JsonDateConverter:
  def parse(date: String, pattern: String): js.Date
  def format(date: js.Date, pattern: String): String

trait JsonFieldConverter[T]:
  def toJson(field: String, v: Any): Any
  def fromJson(field: String, v: Any): Any