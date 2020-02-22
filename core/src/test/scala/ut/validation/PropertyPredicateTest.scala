package ut.validation

import cats.implicits._

import org.scalatest.freespec.AnyFreeSpec

import com.agilogy.validare.validation.Validity.{ Invalid, Valid }
import com.agilogy.validare.validation.predicates.Predicates._
import com.agilogy.validare.validation.{ AtomicPredicate, Conversion, NotPredicate, Property }

class PropertyPredicateTest extends AnyFreeSpec {

  case object isEven extends AtomicPredicate[Int] {
    override def satisfiedBy(value: Int): Boolean = value % 2 == 0

    override def opposite: isOdd.type = isOdd
  }
  case object isOdd extends AtomicPredicate[Int] {
    override def satisfiedBy(value: Int): Boolean = value % 2 == 1

    override def opposite: isEven.type = isEven
  }
  case object toRomanLtX extends Conversion[Int, String] {
    override def transform(value: Int): Option[String] = value match {
      case 1 => Some("i")
      case 2 => Some("ii")
      case 3 => Some("iii")
      case 4 => Some("iv")
      case 5 => Some("v")
      case 6 => Some("vi")
      case 7 => Some("vii")
      case 8 => Some("viii")
      case 9 => Some("ix")
      case _ => None
    }
  }

  "field predicate" - {
    val catName: Property[Cat, String] = at[Cat]("name", _.name)
    val catNameNotEmpty                = catName(nonEmptyS)

    "should validate fields" in {
      assert(catNameNotEmpty(Cat("", 8)) === Invalid(catNameNotEmpty))
      assert(catNameNotEmpty(Cat("Garfield", 8)) === Valid)
    }

    "should have an opposite" in {
      assert(!catNameNotEmpty == catName(isEmptyS))
    }
  }

  "length predicate" in {
    val lengthGt3 = length[String].satisfies(gt(3))
    assert(lengthGt3("abc") === Invalid(lengthGt3))
    assert(lengthGt3("abcd") === Valid)
  }

  "isDefined predicate" - {

    "should validate Option[T]" in {
      assert(defined[String](Some("foo")) === Valid)
      assert(defined[String](None) === Invalid(defined[String]))
    }

    val definedGt3 = defined[Int].satisfies(gt(3))

    "should compose with further validation" in {
      assert(definedGt3(None) === Invalid(defined[Int] && definedGt3))
      assert(definedGt3(Some(2)) === Invalid(definedGt3))
      assert(defined[Int](gt(0) && lt(10))(Some(12)) === Invalid(defined[Int](lt(10))))
    }

    "should have an opposite" in {
      assert(!defined[String] == NotPredicate(defined[String]))
      val notDefinedGt3 = !definedGt3
      assert(notDefinedGt3 == (!defined[Int] || defined[Int].satisfies(!gt(3))))
      assert(notDefinedGt3(Some(5)) === Invalid(!defined[Int] || defined[Int](lteq(3))))
    }
  }

  "ifDefined" - {
    "should define validations to apply to Option[T] only if it is defined" in {
      val gt3 = ifDefined[Int](gt(3))
      assert(gt3(None) === Valid)
      assert(gt3(Some(4)) === Valid)
      assert(gt3(Some(2)) === Invalid(!defined[Int] || defined[Int](gt(3))))
    }
  }

  "intString" in {
    val intStringGt5 = intString(gt(5) && isOdd)
    assert(intStringGt5("9") === Valid)
    assert(intStringGt5("a") === Invalid(intString && intStringGt5))
  }

  "transformed predicates" - {
    val p = intString.satisfies(gt(2) && toRomanLtX.satisfies(length.satisfies(lt(3)) && endsWith("i")))
    "should validate first transformation predicates and keep validating the rest after the second transformation" in {
      assert(p("2") === Invalid(intString.satisfies(gt(2))))
    }
    "should merge all passed and satisfied transformations when complaining about a transformation predicate" in {
      assert(p("3") === Invalid(intString.satisfies(toRomanLtX.satisfies(length.satisfies(lt(3))))))
      assert(p("4") === Invalid(intString.satisfies(toRomanLtX.satisfies(endsWith("i")))))
    }
    "should complain about failed transformations (merging previous passed ones) and about non checked predicates after the transformation" in {
      assert(p("a") === Invalid(intString && p))
      assert(
        p("200000") == Invalid(
          intString.satisfies(toRomanLtX && toRomanLtX.satisfies(length.satisfies(lt(3)) && endsWith("i")))
        )
      )
    }
  }

}
