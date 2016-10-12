package ut.validation

import com.agilogy.validare.validation.Predicate.{False, True}
import com.agilogy.validare.validation.Validity.{Invalid, Valid}
import com.agilogy.validare.validation.{AndPredicate, NotPredicate, OrPredicate, Predicate}
import org.scalatest.FunSpec
import com.agilogy.validare.validation.predicates.Predicates._

@SuppressWarnings(Array("org.wartremover.warts.Any"))
class PredicateTest extends FunSpec {

  val startsWithA = startsWith("a")
  val endsWithA = endsWith("a")
  val startsOrEndsWithA = startsWithA || endsWithA
  val startsAndEndsWithA = startsWithA && endsWithA
  val isEmptyOrBlank = isEmptyS || isBlank
  val allLower = matches("lowercase", "[a-z0-9]+".r)
  val alpha = matches("alpha", "[A-Za-z]+".r)
  val allLowerAlpha = allLower && alpha

  it("should have True and False predicates") {
    assert(True("foo") === Valid)
    assert(False("foo") === Invalid(False))
  }

  it("should pass the disjuntion of predicates if one passes"){
    assert(startsOrEndsWithA("abc") === Valid)
    assert(startsOrEndsWithA("cba") === Valid)
    assert(startsOrEndsWithA("foo") === Invalid(startsOrEndsWithA))
  }

  it("should reduce failed disjuntion to the failing parts"){
    val p = (startsWithA && allLower) || (alpha && endsWithA)
    assert(p("abCDE") === Invalid(allLower || endsWithA))
    assert(p("foo42") === Invalid(startsWithA || (alpha && endsWithA)))
  }

  it("disjuntion with True is always True") {
    assert((True || startsOrEndsWithA) === True)
    assert((startsOrEndsWithA || True) === True)
  }

  it("False is identity for disjuntion") {
    assert((False || startsOrEndsWithA) === startsOrEndsWithA)
    assert((startsOrEndsWithA || False) === startsOrEndsWithA)
  }

  it("conjuntion with False is always false") {
    assert((False && startsOrEndsWithA) === False)
    assert((startsOrEndsWithA && False) === False)
  }

  it("True is identity for conjuntion") {
    assert((True && startsOrEndsWithA) === startsOrEndsWithA)
    assert((startsOrEndsWithA && True) === startsOrEndsWithA)
  }

  it("True is the opposite of False") {
    assert(!True === False)
    assert(!False === True)
  }

  it("should calculate the disjuntion of two predicates") {
    assert((startsWithA || endsWithA) === OrPredicate(Seq(startsWithA, endsWithA)))
    assert((startsOrEndsWithA || isEmptyS) === OrPredicate(Seq(startsWithA, endsWithA, isEmptyS)))
    assert((isEmptyS || startsOrEndsWithA) === OrPredicate(Seq(isEmptyS, startsWithA, endsWithA)))
    assert((isEmptyOrBlank || startsOrEndsWithA) === OrPredicate(Seq(isEmptyS, isBlank, startsWithA, endsWithA)))
    assert(startsOrEndsWithA("abc") === Valid)
    assert(startsOrEndsWithA("cba") === Valid)
    assert(startsOrEndsWithA("foo") === Invalid(startsOrEndsWithA))
  }

  it("should calculate the conjuntion of two predicates") {
    assert((startsWithA && endsWithA) === AndPredicate(Seq(startsWithA, endsWithA)))
    assert((startsAndEndsWithA && allLower) === AndPredicate(Seq(startsWithA, endsWithA, allLower)))
    assert((allLower && startsAndEndsWithA) === AndPredicate(Seq(allLower, startsWithA, endsWithA)))
    assert((allLowerAlpha && startsAndEndsWithA) === AndPredicate(Seq(allLower, alpha, startsWithA, endsWithA)))
    assert(startsAndEndsWithA("abracadabra") === Valid)
    assert(startsAndEndsWithA("abracadabro") === Invalid(endsWithA))
    assert(startsAndEndsWithA("obracadabra") === Invalid(startsWithA))
    assert(startsAndEndsWithA("foo") === Invalid(startsWithA && endsWithA))
  }

  it("should negate atomic predicates with opposites") {
    assert(!isEmptyS === nonEmptyS)
    assert(!nonEmptyS === isEmptyS)
  }

  it("should negate atomic predicates without opposites") {
    assert(!startsWithA === NotPredicate(startsWithA))
    assert((!startsWithA) ("abc") === Invalid(!startsWithA))
    assert((!startsWithA) ("foo") === Valid)
  }

  it("should negate conjuntion predicates") {
    assert(!startsAndEndsWithA === (!startsWithA || !endsWithA))
  }

  it("should negate disjuntion predicates") {
    assert(!startsOrEndsWithA === (!startsWithA && !endsWithA))
  }

  it("should eliminate double negation") {
    assert(!(!startsAndEndsWithA) === startsAndEndsWithA)
    assert(!(!startsWithA) === startsWithA)
  }

  it("should build implies predicates") {
    assert((nonEmptyS implies allLowerAlpha) === (!nonEmptyS || allLowerAlpha))
  }

  it("should be contravariant") {
    trait Animal{
      val name:String
    }
    case class Dog(name:String) extends Animal
    case class Cat(name:String) extends Animal
    val nonEmptyName:Predicate[Animal] = at[Animal]("name",_.name)(nonEmptyS)
    val dogName:Predicate[Dog] = at[Dog]("name",_.name)(endsWith("y"))
    val dogValidation: Predicate[Dog] = nonEmptyName && dogName
    val catValidation: Predicate[Cat] = nonEmptyName
    assert(dogValidation(Dog("Snoopy")) === Valid)
    assert(dogValidation(Dog("")) === Invalid(nonEmptyName && dogName))
    assert(dogValidation(Dog("Garfield")) === Invalid(dogName))
    assert(catValidation(Cat("")) === Invalid(nonEmptyName))
    assert(catValidation(Cat("Garfield")) === Valid)


  }

}
