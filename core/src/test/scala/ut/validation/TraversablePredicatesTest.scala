package ut.validation

import com.agilogy.validare.validation.PositionPredicate
import com.agilogy.validare.validation.Validity.Invalid
import org.scalatest.FreeSpec
import com.agilogy.validare.validation.predicates.Predicates._

class TraversablePredicatesTest extends FreeSpec{

  val startsWithA = startsWith("a")
  val endsWithA = endsWith("a")

  "forAll" - {
    assert(forAll(startsWithA)(Seq("abc","foo","abd")) === Invalid(PositionPredicate(1,startsWithA)))
    assert(forAll(startsWithA && endsWithA)(Seq("abc","foo","abda")) ===
      Invalid(PositionPredicate(0,endsWithA) && PositionPredicate(1, startsWithA && endsWithA)))

  }

}
