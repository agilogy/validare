package com.agilogy.validare.parsing.parsers

import com.agilogy.validare.parsing.AtomicParser
import com.agilogy.validare.parsing.Validated.{Invalid, Parsed, Valid}

import scala.util.Try

trait SimpleStringParsers {

  case object parseBoolean extends AtomicParser[String, Boolean] {
    override def apply(input: String): Parsed[Boolean] = Try(Valid(input.toBoolean)).getOrElse(Invalid(this))
  }

  case object parseByte extends AtomicParser[String, Byte] {
    override def apply(input: String): Parsed[Byte] = Try(Valid(input.toByte)).getOrElse(Invalid(this))
  }

  case object parseShort extends AtomicParser[String, Short] {
    override def apply(input: String): Parsed[Short] = Try(Valid(input.toShort)).getOrElse(Invalid(this))
  }

  case object parseInt extends AtomicParser[String, Int] {
    override def apply(input: String): Parsed[Int] = Try(Valid(input.toInt)).getOrElse(Invalid(this))
  }

  case object parseLong extends AtomicParser[String, Long] {
    override def apply(input: String): Parsed[Long] = Try(Valid(input.toLong)).getOrElse(Invalid(this))
  }

  case object parseFloat extends AtomicParser[String, Float] {
    override def apply(input: String): Parsed[Float] = Try(Valid(input.toFloat)).getOrElse(Invalid(this))
  }

  case object parseFloatExact extends AtomicParser[String, Float] {
    override def apply(input: String): Parsed[Float] = {
      Try {
        val bd = BigDecimal(input)
        val res = bd.toFloat
        if (BigDecimal(res.toDouble) == bd) Valid(res)
        else Invalid(this)
      }.getOrElse(Invalid(this))
    }
  }

  case object parseDouble extends AtomicParser[String, Double] {
    override def apply(input: String): Parsed[Double] = Try(Valid(input.toDouble)).getOrElse(Invalid(this))
  }

  case object parseDoubleExact extends AtomicParser[String, Double] {
    override def apply(input: String): Parsed[Double] = {
      Try {
        val bd = BigDecimal(input)
        val res = bd.toDouble
        if (BigDecimal(res) == bd) Valid(res)
        else Invalid(this)
      }.getOrElse(Invalid(this))
    }
  }

  case object parseBigDecimal extends AtomicParser[String, BigDecimal] {
    override def apply(input: String): Parsed[BigDecimal] = Try(Valid(BigDecimal(input))).getOrElse(Invalid(this))
  }

}
