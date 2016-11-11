package ut.validation

import com.agilogy.validare.validation.{Property, NotPredicate}
import com.agilogy.validare.validation.Validity.{Invalid, Valid}
import org.scalatest.FreeSpec
import com.agilogy.validare.validation.predicates.Predicates._

final case class Cat(name:String, age:Int)

class PropertyPredicateTest extends FreeSpec{

  "field predicate" - {
    val catName: Property[Cat, String] = at[Cat]("name",_.name)
    val catNameNotEmpty = catName(nonEmptyS)

    "should validate fields" in {
      assert(catNameNotEmpty(Cat("",8)) === Invalid(catNameNotEmpty))
      assert(catNameNotEmpty(Cat("Garfield",8)) === Valid)
    }

    "should have an opposite" in {
      assert(!catNameNotEmpty === catName(isEmptyS))
    }
  }

  "length predicate" - {
    val lengthGt3 = length[String].validate(gt(3))
    assert(lengthGt3("abc") === Invalid(lengthGt3))
    assert(lengthGt3("abcd") === Valid)
  }

  "isDefined predicate" - {

    "should validate Option[T]" in {
      assert(isDefined[String](Some("foo")) === Valid)
      assert(isDefined[String](None) === Invalid(isDefined[String]))
    }

    val definedGt3 = isDefined[Int](gt(3))

    "should compose with further validation" in {
      assert(definedGt3(None) === Invalid(isDefined[Int] && definedGt3))
      //TODO: Not sure. Maybe we should provide information on the transformation applied
      assert(definedGt3(Some(2)) === Invalid(definedGt3))
      assert(isDefined[Int](gt(0) && lt(10))(Some(12)) === Invalid(isDefined[Int](lt(10))))
    }

    "should have an opposite" in {
      assert(!isDefined[String] === notDefined[String])
      val notDefinedGt3 = !definedGt3
      assert(notDefinedGt3 === (notDefined[Int] || isDefined[Int](!gt(3))))
      assert(notDefinedGt3(Some(5)) === Invalid(notDefined[Int] || isDefined[Int](lteq(3))))
    }
  }

  "ifDefined" - {
    "should define validations to apply to Option[T] only if it is defined" in {
      val gt3 = ifDefined[Int](gt(3))
      assert(gt3(None) === Valid)
      assert(gt3(Some(4)) === Valid)
      assert(gt3(Some(2)) === Invalid(!isDefined[Int] || isDefined[Int](gt(3))))
    }
  }

}
