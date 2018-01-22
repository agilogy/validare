package ut.validation

import com.agilogy.validare.validation.Validity
import com.agilogy.validare.validation.Validity.{Invalid, Valid}
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
    assert(nonEmptyT(List.empty[Int]) === Validity.Invalid(nonEmptyT[List[Int]]))
  }

  it("should validate a collection is empty") {
    assert(isEmptyT(List.empty[Int]) === Validity.Valid)
    assert(isEmptyT(List(23)) === Validity.Invalid(isEmptyT[List[Int]]))
  }

  it("should calculate the opposite of isEmpty and nonEmpty") {
    assert(!isEmptyS === nonEmptyS)
    assert(!nonEmptyS === isEmptyS)
    assert(!isEmptyT[List[Int]] === nonEmptyT[List[Int]])
    assert(!nonEmptyT[List[Int]] === isEmptyT[List[Int]])
  }

  it("should build predicates on length"){
    //TODO: Try to avoid having to write .apply
    val lengthGt2 = length[Seq[Int]].apply(gt(2))
    assert(lengthGt2(Seq(1,2,3)) === Valid)
    assert(lengthGt2(Seq(1,2)) === Invalid(lengthGt2))
  }
}
