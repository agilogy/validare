package ut

import com.agilogy.validare.parsing.parsers.StringParsers._
import com.agilogy.validare.parsing.And
import com.agilogy.validare.parsing.Validated.{Invalid, Valid}
import com.agilogy.validare.validation.{Predicate, Validity}
import org.scalatest.FunSpec

class ValidationTest extends FunSpec {

  it("should trim strings") {
    assert(trim(" a  ") === Valid("a"))
  }

  it("should compose a parser with a verification") {
    assert((trim andThen nonEmpty[String]) ("  ") === Invalid(nonEmpty[String]))
  }

  it("should compose a parser with multiple verifications") {
    val v = trim andThen startsWith("a") && endsWith("z")
    assert(v("   a   d") === Invalid(endsWith("z")))
    assert(v("   r   z") === Invalid(startsWith("a")))
    assert(v("   r   d") === Invalid(startsWith("a") && endsWith("z")))
  }

  it("should parse and validate values") {
    val positiveInt= parseInt andThen gt(0)
    assert(positiveInt("foo") === Invalid(parseInt))
    assert(positiveInt("-2") === Invalid(gt(0)))
  }

  it("should chain andThen's") {
    //TODO: Avoid having to tell Scala the fucking type of v
    val v: And[String, String] = trim andThen trim andThen trim andThen nonEmpty[String]
    assert(v("   ") === Invalid(nonEmpty[String]))
  }

  case class Person(name: String, age: Int)

  it("should validate fields") {
    //TODO: Functions can be compared, and they are part of FieldPredicate's equals
    val nameF = (p: Person) => p.name
    val ageF = (p: Person) => p.age
    val v: Predicate[Person] =
      at[Person]("name", nameF)(nonEmptyS) &&
        at[Person]("age", ageF)(gt(18) && lt(150))
    assert(v(Person("", 3)) === Validity.Invalid(at[Person]("name", nameF)(nonEmptyS) && at[Person]("age", ageF)(gt(18))))
    assert(v(Person("", 33)) === Validity.Invalid(at[Person]("name", nameF)(nonEmptyS)))

  }


  //  implicit val intComparator: Comparator[Int] = new Comparator[Int] {
  //    override def compare(o1: Int, o2: Int): Int = o1 - o2
  //  }

  //  implicit val errorLevelComparator:Comparator[ErrorLevel] = new Comparator[ErrorLevel] {
  //    import ErrorLevel._
  //    override def compare(o1: ErrorLevel, o2: ErrorLevel): Int = (o1, o2) match {
  //      case (Error(e1), Error(e2)) => e1 - e2
  //      case (Error(_), _) => Integer.MAX_VALUE
  //      case (Warning(w1), Warning(w2)) => w1 - w2
  //      case (Warning(_), _) => Integer.MIN_VALUE
  //    }
  //  }
  //
  //  it("should evaluate the comparison"){
  //    val gt3 = GT(3)
  //    assert(gt3.check(3) === Some(AtomicValidationError(3,gt3)))
  //    assert(gt3.check(4) === None)
  //  }
  //
  //  it("should evalute the comparison with hierarchies"){
  //    val e0 = ErrorLevel.Error(0)
  //    val gtE0 = GT[ErrorLevel](e0)
  //    val w999 = ErrorLevel.Warning(999)
  //    assert(gtE0.check(w999) === Some(AtomicValidationError(w999,gtE0)))
  //    assert(gtE0.check(e0) === Some(AtomicValidationError(e0,gtE0)))
  //    assert(gtE0.check(ErrorLevel.Error(1)) === None)
  //  }
  //
  //  it("should build and validations"){
  //    val between3And5 = GT(2) and LT(6)
  //    assert(between3And5.check(2) === Some(AndValidationError(2,between3And5,Seq(GT(2)))))
  //    assert(between3And5.check(6) === Some(AndValidationError(6,between3And5,Seq(LT(6)))))
  //    assert(between3And5.check(4) === None)
  //  }

}
