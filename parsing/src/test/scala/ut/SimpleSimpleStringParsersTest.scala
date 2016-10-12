package ut

import com.agilogy.validare.parsing.Validated.{Invalid, Valid}
import org.scalatest.FunSpec
import com.agilogy.validare.parsing.parsers.StringParsers._

class SimpleSimpleStringParsersTest extends FunSpec {

  it("should parse booleans") {
    assert(parseBoolean("foo") === Invalid(parseBoolean))
    assert(parseBoolean("true") === Valid(true))
  }

  it("should parse bytes") {
    assert(parseByte("foo") === Invalid(parseByte))
    assert(parseByte("123456789") === Invalid(parseByte))
    assert(parseByte("2") === Valid(2.toByte))
  }

  it("should parse shorts") {
    assert(parseShort("foo") === Invalid(parseShort))
    assert(parseShort("123456789") === Invalid(parseShort))
    assert(parseShort("2") === Valid(2.toShort))
  }

  it("should parse ints") {
    assert(parseInt("foo") === Invalid(parseInt))
    assert(parseInt("123456789000") === Invalid(parseInt))
    assert(parseInt("2") === Valid(2))
  }

  it("should parse longs") {
    assert(parseLong("foo") === Invalid(parseLong))
    assert(parseLong("123456789000000000000000") === Invalid(parseLong))
    assert(parseLong("2") === Valid(2.toLong))
  }

  it("should parse floats") {
    assert(parseFloat("foo") === Invalid(parseFloat))
    assert(parseFloat("2") === Valid(2.toFloat))
    assert(parseFloatExact("foo") === Invalid(parseFloatExact))
    assert(parseFloatExact("123456789.123456789") === Invalid(parseFloatExact))
    assert(parseFloatExact("2.0") === Valid(2.0.toFloat))
  }

  it("should parse doubles") {
    assert(parseDouble("foo") === Invalid(parseDouble))
    assert(parseDouble("2") === Valid(2.toDouble))
    assert(parseDoubleExact("foo") === Invalid(parseDoubleExact))
    assert(parseDoubleExact("123456789.123456789") === Invalid(parseDoubleExact))
    assert(parseDoubleExact("2.0") === Valid(2.0))
  }

  it("should parse BigDecimals") {
    assert(parseBigDecimal("foo") === Invalid(parseBigDecimal))
    assert(parseBigDecimal("2") === Valid(BigDecimal(2)))
  }


}
