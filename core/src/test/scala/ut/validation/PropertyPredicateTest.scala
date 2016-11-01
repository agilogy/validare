package ut.validation

import com.agilogy.validare.validation.Validity.{Invalid, Valid}
import org.scalatest.FreeSpec
import com.agilogy.validare.validation.predicates.Predicates._

final case class Cat(name:String, age:Int)

class PropertyPredicateTest extends FreeSpec{

  "field predicate" - {
    val catName = at[Cat]("name",_.name)
    val catNameNotEmpty = catName(nonEmptyS)

    "should validate fields" in {
      assert(catNameNotEmpty(Cat("",8)) === Invalid(catNameNotEmpty))
      assert(catNameNotEmpty(Cat("Garfield",8)) === Valid)
    }

    "should have an opposite" in {
      assert(!catNameNotEmpty === catName(isEmptyS))
    }
  }

}
