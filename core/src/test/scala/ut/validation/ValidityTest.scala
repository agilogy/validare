package ut.validation

import org.scalatest.funspec.AnyFunSpec

import com.agilogy.validare.validation.Validity.{ Invalid, Valid }
import com.agilogy.validare.validation.predicates.Predicates._

class ValidityTest extends AnyFunSpec {

  private val startsWithA = startsWith("a")
  private val endsWithZ   = endsWith("z")

  it("should tell if it is valid") {
    assert(Valid.isValid === true)
    assert(Invalid(startsWithA).isValid === false)
  }

  it("should calculate the and of two validities") {
    assert((Valid && Valid) === Valid)
    assert((Valid && Invalid(startsWithA)) === Invalid(startsWithA))
    assert((Invalid(startsWithA) && Valid) === Invalid(startsWithA))
    assert((Invalid(startsWithA) && Invalid(endsWithZ)) === Invalid(startsWithA && endsWithZ))
    assert((Invalid(endsWithZ) && Invalid(startsWithA)) === Invalid(endsWithZ && startsWithA))
  }

  it("should calculate the or of two validities") {
    assert((Valid || Valid) === Valid)
    assert((Valid || Invalid(startsWithA)) === Valid)
    assert((Invalid(startsWithA) || Valid) === Valid)
    assert((Invalid(startsWithA) || Invalid(endsWithZ)) === Invalid(startsWithA || endsWithZ))
    assert((Invalid(endsWithZ) || Invalid(startsWithA)) === Invalid(endsWithZ || startsWithA))
  }

}
