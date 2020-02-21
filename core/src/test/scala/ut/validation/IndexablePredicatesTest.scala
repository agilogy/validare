package ut.validation

import org.scalatest.freespec.AnyFreeSpec

import com.agilogy.validare.validation.Predicate
import com.agilogy.validare.validation.Validity.{ Invalid, Valid }
import com.agilogy.validare.validation.predicates.Predicates._

class IndexablePredicatesTest extends AnyFreeSpec {

  private val startsWithA = startsWith("a")
  private val endsWithA   = endsWith("a")

  "forAll" - {
    val p: Predicate[Seq[String]] = forAll(startsWithA)
    "should check all the values of any traversable" in {
      assert(p(Seq("abc", "foo", "abd")) === Invalid(atPos[Seq, String](1).satisfies(startsWithA)))
    }
    "should reduce each value to the failing part" in {
      val p2: Predicate[Seq[String]] = forAll(startsWithA && endsWithA)
      assert(
        p2(Seq("abc", "foo", "abda")) ===
          Invalid(atPos(0).satisfies(endsWithA) && atPos(1).satisfies(startsWithA && endsWithA))
      )
    }
    "should have an opposite" in {
      assert(!p === exists(!startsWithA))
    }
  }
  "exists" - {
    val p: Predicate[Seq[String]] = exists(startsWithA)
    "should check the values of a traversable" in {
      assert(p(Seq("abc", "adr")) === Valid)
      assert(p(Seq("foo", "bar")) === Invalid(atPos(0).satisfies(startsWithA) || atPos(1).satisfies(startsWithA)))
    }
    "should fail when no elements exist at all" in {
      assert(p(Seq.empty) === Invalid(p))
    }
    "should reduce each value to the failing part" in {
      val p2: Predicate[Seq[String]] = exists(startsWithA && endsWithA)
      assert(
        p2(Seq("abc", "foo")) ===
          Invalid(atPos(0).satisfies(endsWithA) || atPos(1).satisfies(startsWithA && endsWithA))
      )
    }
    "should have an opposite" in {
      assert(!p === forAll(!startsWithA))
    }
  }
  "at" - {
    val p: Predicate[Seq[String]] = atPos(1).satisfies(isEmptyS)
    "should check the value at a particular position" in {
      assert(p(Seq("abc", "")) === Valid)
      assert(p(Seq("abc", "a")) === Invalid(p))
//      assert(p(Seq("abc")) === Invalid(length[Seq[String]].satisfies(gteq(1)) && atPos(1).satisfies(isEmptyS)))
      assert(p(Seq("abc")) === Invalid(AtPosition[Seq, String](1) && atPos(1).satisfies(isEmptyS)))
    }
    "should have an opposite" in {
//      assert(p.opposite === (length[Seq[String]].satisfies(lt(1)) || atPos(1).satisfies(nonEmptyS)))
      assert(p.opposite === (!AtPosition[Seq, String](1) || atPos(1).satisfies(nonEmptyS)))
    }
  }

}
