package ut.validation

import com.agilogy.validare.validation.{PositionPredicate, Predicate}
import com.agilogy.validare.validation.Validity.Invalid
import org.scalatest.FreeSpec
import com.agilogy.validare.validation.predicates.Predicates._

class TraversablePredicatesTest extends FreeSpec{

  val startsWithA = startsWith("a")
  val endsWithA = endsWith("a")

  "forAll" - {
    "should check all the values of any traversable" in {
      val p:Predicate[Seq[String]] = forAll(startsWithA)
      assert(p(Seq("abc", "foo", "abd")) === Invalid(PositionPredicate[Seq,String](1, startsWithA)))
    }
    "should reduce each value to the failing part" in {
      val p:Predicate[Seq[String]] = forAll(startsWithA && endsWithA)
      assert(p(Seq("abc", "foo", "abda")) ===
        Invalid(PositionPredicate(0, endsWithA) && PositionPredicate(1, startsWithA && endsWithA)))
    }
  }
  "exists" - {
    "should check the values of a traversable" in {
      val p:Predicate[Seq[String]] = exists(startsWithA)
      assert(p(Seq("foo","bar")) === Invalid(PositionPredicate(0,startsWithA) || PositionPredicate(1,startsWithA)))
    }
    "should reduce each value to the failing part" in {
      val p:Predicate[Seq[String]] = exists(startsWithA && endsWithA)
      assert(p(Seq("abc", "foo")) ===
        Invalid(PositionPredicate(0, endsWithA) || PositionPredicate(1, startsWithA && endsWithA)))
    }
  }

  //TODO: Fix this
//  "forAll and exists are opposites" in {
//    assert(!forAll(startsWithA) === exists(!startsWithA))
//    assert(!exists(startsWithA) === forAll(!startsWithA))
//  }
}
