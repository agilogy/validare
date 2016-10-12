package ut.validation

import com.agilogy.validare.validation.Validity
import com.agilogy.validare.validation.predicates.Predicates._
import org.scalatest.FunSpec

class HasLengthPredicatesTest extends FunSpec {

  it("should validate a string is not empty") {
    assert(nonEmptyS("foo") === Validity.Valid)
    assert(nonEmptyS("") === Validity.Invalid(nonEmptyS))
  }

  it("should validate a string is empty") {
    assert(isEmptyS("") === Validity.Valid)
    assert(isEmptyS("foo") === Validity.Invalid(isEmptyS))
  }

  it("should validate a collection is not empty") {
    assert(nonEmptyT(List(34)) === Validity.Valid)
    assert(nonEmptyT(List()) === Validity.Invalid(nonEmptyT))
  }

  it("should validate a collection is empty") {
    assert(isEmptyT(List()) === Validity.Valid)
    assert(isEmptyT(List(23)) === Validity.Invalid(isEmptyT))
  }

  it("should calculate the opposite of isEmpty and nonEmpty") {
    assert(!isEmptyS === nonEmptyS)
    assert(!nonEmptyS === isEmptyS)
    assert(!isEmptyT[List[Int]] === nonEmptyT[List[Int]])
    assert(!nonEmptyT[List[Int]] === isEmptyT[List[Int]])
  }
}
