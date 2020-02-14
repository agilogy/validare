package ut.validation

import org.scalatest.funspec.AnyFunSpec

import com.agilogy.validare.validation.NotPredicate
import com.agilogy.validare.validation.Validity._
import com.agilogy.validare.validation.predicates.Predicates._

class StringPredicatesTest extends AnyFunSpec {

  it("should validate a string matches a regex") {
    val isAsciiAlpha = matches("ascii-alpha", "[a-zA-Z]+".r)
    assert(isAsciiAlpha("abc") === Valid)
    assert(isAsciiAlpha("abc3") === Invalid(isAsciiAlpha))
  }

  it("should get the opposite of a regex matching predicate") {
    val isAsciiAlpha = matches("ascii-alpha", "[a-zA-Z]+".r)
    assert(!isAsciiAlpha === NotPredicate(isAsciiAlpha))

  }

  it("should validate a string not being blank") {
    assert(nonBlank("foo") === Valid)
    assert(nonBlank("  \t") === Invalid(nonBlank))
  }

  it("should validate a string being blank") {
    assert(isBlank("foo") === Invalid(isBlank))
    assert(isBlank("  \t") === Valid)
  }

  it("should calculate the opposite of isBlank and notBlank") {
    assert(!isBlank === nonBlank)
    assert(!nonBlank === isBlank)
  }

  it("should validate a string starts with a preffix") {
    val startsWithA = startsWith("A")
    assert(startsWithA("Abracadabra") === Valid)
    assert(startsWithA("Barcelona") === Invalid(startsWithA))
  }

  it("should validate a string ends with a suffix") {
    val endsWithA = endsWith("A")
    assert(endsWithA("AbracadabrA") === Valid)
    assert(endsWithA("Foo") === Invalid(endsWithA))
  }

}
