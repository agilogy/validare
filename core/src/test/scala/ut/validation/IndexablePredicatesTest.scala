package ut.validation

import com.agilogy.validare.validation.Predicate.False
import com.agilogy.validare.validation.{PositionPredicate, Predicate}
import com.agilogy.validare.validation.Validity.{Invalid, Valid}
import org.scalatest.FreeSpec
import com.agilogy.validare.validation.predicates.Predicates._

class IndexablePredicatesTest extends FreeSpec{

  val startsWithA = startsWith("a")
  val endsWithA = endsWith("a")

  "forAll" - {
    val p:Predicate[Seq[String]] = forAll(startsWithA)
    "should check all the values of any traversable" in {
      assert(p(Seq("abc", "foo", "abd")) === Invalid(atPosition[Seq,String](1, startsWithA)))
    }
    "should reduce each value to the failing part" in {
      val p2:Predicate[Seq[String]] = forAll(startsWithA && endsWithA)
      assert(p2(Seq("abc", "foo", "abda")) ===
        Invalid(PositionPredicate(0, endsWithA) && atPosition(1, startsWithA && endsWithA)))
    }
    "should have an opposite" in {
      assert(!p === exists(!startsWithA))
    }
  }
  "exists" - {
    val p:Predicate[Seq[String]] = exists(startsWithA)
    "should check the values of a traversable" in {
      assert(p(Seq("abc","adr")) === Valid)
      assert(p(Seq("foo","bar")) === Invalid(PositionPredicate(0,startsWithA) || atPosition(1,startsWithA)))
    }
    "should fail when no elements exist at all" in {
      assert(p(Seq.empty) === Invalid(p))
    }
    "should reduce each value to the failing part" in {
      val p2:Predicate[Seq[String]] = exists(startsWithA && endsWithA)
      assert(p2(Seq("abc", "foo")) ===
        Invalid(PositionPredicate(0, endsWithA) || atPosition(1, startsWithA && endsWithA)))
    }
    "should have an opposite" in {
      assert(!p === forAll(!startsWithA))
    }
  }
  "at" - {
    val p:Predicate[Seq[String]] = atPosition(1,isEmptyS)
    "should check the value at a particular position" in {
      assert(p(Seq("abc","")) === Valid)
      assert(p(Seq("abc","a")) === Invalid(p))
      // TODO: Fix this. It should fail at size >= 2
      assert(p(Seq("abc")) === Invalid(False))
    }
    "should have an opposite" in {
      assert(p.opposite === atPosition[Seq,String](1,nonEmptyS))
    }
  }

}
