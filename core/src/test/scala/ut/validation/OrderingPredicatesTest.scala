package ut.validation

import com.agilogy.validare.validation.Validity
import com.agilogy.validare.validation.predicates.Predicates._
import org.scalatest.FunSpec

class OrderingPredicatesTest extends FunSpec {

  it("should validate gt") {
    val gt3 = gt(3)
    assert(gt3(4) === Validity.Valid)
    assert(gt3(3) === Validity.Invalid(gt3))
  }

  it("should validate gteg") {
    val gteq3 = gteq(3)
    assert(gteq3(4) === Validity.Valid)
    assert(gteq3(3) === Validity.Valid)
    assert(gteq3(2) === Validity.Invalid(gteq3))
  }

  it("should validate lt") {
    val lt3 = lt(3)
    assert(lt3(2) === Validity.Valid)
    assert(lt3(3) === Validity.Invalid(lt3))
  }

  it("should validate lteg") {
    val lteq3 = lteq(3)
    assert(lteq3(2) === Validity.Valid)
    assert(lteq3(3) === Validity.Valid)
    assert(lteq3(4) === Validity.Invalid(lteq3))
  }

  it("should calculate the opposites of lteq and gt") {
    val lteq3 = lteq(3)
    val gt3 = gt(3)
    assert(!lteq3 === gt3)
    assert(!gt3 === lteq3)
  }

  it("should calculate the opposites of gteq and lt") {
    val gteq3 = gteq(3)
    val lt3 = lt(3)
    assert(!gteq3 === lt3)
    assert(!lt3 === gteq3)
  }

}
