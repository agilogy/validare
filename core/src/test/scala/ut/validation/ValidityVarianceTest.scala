package ut.validation

import com.github.ghik.silencer.silent
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

@silent("NonUnitStatements")
class ValidityVarianceTest extends AnyFunSpec with Matchers {

  it("should be contravariant") {
    import com.agilogy.validare.validation.Validity
    import com.agilogy.validare.validation.Validity._
    "def invalidAnimal:Invalid[Animal] = ???; val catValidity:Validity[Cat] = invalidAnimal" should compile
    "def validAny:Valid.type = Valid; val catValidity:Validity[Cat] = validAny" should compile
    "def invalidCat:Invalid[Cat] = ???; val animalValidity:Validity[Animal] = invalidCat" shouldNot typeCheck
    "def validAny:Valid.type = Valid; val animalValidity:Validity[Animal]" shouldNot typeCheck
  }

}
