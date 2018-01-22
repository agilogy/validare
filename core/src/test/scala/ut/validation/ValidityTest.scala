package ut.validation

import com.agilogy.validare.validation.Validity
import com.agilogy.validare.validation.predicates.Predicates._
import com.agilogy.validare.validation.Validity.{Invalid, Valid}
import org.scalatest.{FunSpec, Matchers}

@SuppressWarnings(Array("org.wartremover.warts.Any"))
class ValidityTest extends FunSpec with Matchers{

  val startsWithA = startsWith("a")
  val endsWithZ = endsWith("z")

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

  it("should be contravariant"){
    "def invalidAnimal:Invalid[Animal] = ???; val catValidity:Validity[Cat] = invalidAnimal" should compile
    "def validAny:Valid.type = Valid; val catValidity:Validity[Cat] = validAny" should compile
    "def invalidCat:Invalid[Cat] = ???; val animalValidity:Validity[Animal] = invalidCat" shouldNot typeCheck
    "def validAny:Valid.type = Valid; val animalValidity:Validity[Animal]" shouldNot typeCheck


  }

}
