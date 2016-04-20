package ut

import com.agilogy.validare._
import org.scalatest.FunSpec
import Context.__

class ValidatedTest extends FunSpec {

  val beOddMsg = "be odd"
  val notOdd = itShould(beOddMsg)
  val beANumberMsg: String = "be a number"
  val notANumber = itShould(beANumberMsg)
  val empty = itShould("not be empty")
  val notSmallerThan10 = itShould("be smaller than", "max" -> "10")
  val notDivisibleBy3 = itShould("be divisible", "by" -> "3")
  val validFive: Validated[Int] = valid(5)
  val invalidFour: Validated[Int] = invalid(4, notOdd)
  val illegalInt: Validated[Int] = illegal(notANumber)

  describe("fold") {

    it("should fold over valid values") {
      assert(validFive.fold(_ => "boo!", _ + "bar") === "5bar")

    }

    it("should fold over invalid values") {
      assert(invalidFour.fold(_.apply(__).map(_.message), _ => "ouch!") === Seq(beOddMsg))
    }

    it("should fold over illegal values") {
      assert(illegalInt.fold(_ (__).map(_.message), _ => "ouch!") === Seq(beANumberMsg))
    }
  }

  describe("isSuccess and isError") {

    it("should consider Valid a success") {
      assert(validFive.isSuccess === true)
      assert(validFive.isError === false)
    }

    it("should consider invalid an error") {
      assert(invalidFour.isSuccess === false)
      assert(invalidFour.isError === true)
    }

    it("should consider illegal an error") {
      assert(illegalInt.isSuccess === false)
      assert(illegalInt.isError === true)
    }

  }

  describe("foreach") {

    it("should execute side effects for Valid values only") {
      val sb = new StringBuffer()
      @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.NonUnitStatements"))
      val log = { s: Int => sb.append(s); () }
      invalidFour.foreach(log)
      assert(sb.toString === "")
      illegalInt.foreach(log)
      assert(sb.toString === "")
      validFive.foreach(log)
      assert(sb.toString === "5")
    }

  }

  describe("orElse") {

    it("should keep Valid values unchanged") {
      assert(validFive.orElse(Valid(23)) === validFive)
    }

    it("should use the default value for invalid values") {
      assert(invalidFour.orElse(Valid(23)) === Valid(23))
    }

    it("should use the default value for illegal values") {
      assert(illegalInt.orElse(Valid(23)) === Valid(23))
    }

  }

  describe("getOrElse") {

    it("should return the valid value") {
      assert(validFive.getOrElse(33) === 5)
    }

    it("should return the default value for invalid values") {
      assert(invalidFour.getOrElse(33) === 33)
    }

    it("should return the default value for illegal values") {
      assert(illegalInt.getOrElse(33) === 33)
    }

  }

  describe("exists") {


    @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.NonUnitStatements"))
    def f(sb: StringBuffer): (Int => Boolean) = {
      s =>
        sb.append("invoked!")
        s % 5 == 0
    }

    it("should check if the value matches the predicate for valid values") {
      val sb = new StringBuffer()
      assert(validFive.exists(f(sb)) === true)
      assert(valid(7).exists(f(sb)) === false)
      assert(sb.toString === "invoked!invoked!")
    }

    it("should return false directly for invalid or illegal values") {
      val sb = new StringBuffer()
      assert(invalidFour.exists(f(sb)) === false)
      assert(illegalInt.exists(f(sb)) === false)
      assert(sb.toString === "")
    }

  }

  describe("toEither") {

    it("should convert a valid value to right") {
      assert(validFive.toEither === Right(5))
    }

    it("should convert a invalid and illegal values to left") {
      assert(invalidFour.toEither === Left(invalidFour.validity))
      assert(illegalInt.toEither === Left(illegalInt.validity))
    }

  }

  describe("toOption") {

    it("should convert a valid value to Some") {
      assert(validFive.toOption === Some(5))
    }

    it("should convert a invalid and illegal values to None") {
      assert(invalidFour.toOption === None)
      assert(illegalInt.toOption === None)
    }

  }

  describe("combine") {

    val f: (Int, Int) => Int = _ - _

    it("should return illegal whenever we have an illegal value, while accumulating errors") {

      assert(validFive.combine(illegalInt)(f) === illegalInt)
      assert(illegalInt.combine(validFive)(f) === illegalInt)

      assert(invalidFour.at("a").combine(illegalInt.at("b"))(f) === illegal(__ / "a" -> notOdd, __ / "b" -> notANumber))
      assert(illegalInt.at("a").combine(invalidFour.at("b"))(f) === illegal(__ / "a" -> notANumber, __ / "b" -> notOdd))

      assert(illegal(empty).at("a").combine(illegalInt.at("b"))(f) === illegal(__ / "a" -> empty, __ / "b" -> notANumber))
      assert(illegalInt.at("a").combine(illegal(empty).at("b"))(f) === illegal(__ / "a" -> notANumber, __ / "b" -> empty))
    }

    it("should return invalid whenever we have some invalid value and no illegal value, while accumulating errors") {
      val invalid13 = invalid(13, notDivisibleBy3, notSmallerThan10)
      assert(validFive.at("a").combine(invalid13.at("b"))(f) === invalid(5-13, __ / "b" -> notDivisibleBy3, __  / "b" -> notSmallerThan10))
      assert(invalid13.at("a").combine(validFive.at("b"))(f) === invalid(13-5, __ / "a" -> notDivisibleBy3, __ / "a" -> notSmallerThan10))

      assert(invalidFour.at("a").combine(invalid13.at("b"))(f) === invalid(4 - 13, __ / "a" -> notOdd, __ / "b" -> notDivisibleBy3, __ / "b" -> notSmallerThan10))
      assert(invalid13.at("a").combine(invalidFour.at("b"))(f) === invalid(13 - 4, __ / "a" -> notDivisibleBy3, __ / "a" -> notSmallerThan10,  __ / "b" -> notOdd))
    }

    it("should return valid only if both validations are valid") {
      val validThree = Valid(3)
      assert(validFive.combine(validThree)(f) === Valid(5-3))
      assert(validThree.combine(validFive)(f) === Valid(3-5))
    }

  }

  describe("flatMap") {

    val notInteresting = itShould("be more interesting")
    val wrongType = itShould("return Int")
    val validF: Int => Validated[Int] = s => Valid(s - 2)
    val invalidF: Int => Validated[Int] = s => invalid(s + 1, notInteresting)
    val illegalF: Int => Validated[Int] = _ => illegal(wrongType)

    it("should apply the function to valid values") {
      assert(validFive.flatMap(validF) === Valid(5-2))
      assert(validFive.flatMap(invalidF) === invalid(5+1, notInteresting))
      assert(validFive.flatMap(illegalF) === illegal(wrongType))
    }

    it("should apply the function to invalid values and accumulate errors") {
      assert(invalidFour.flatMap(validF) === invalid(4-2, notOdd))
      assert(invalidFour.flatMap(invalidF) === invalid(4+1, notOdd, notInteresting))
      assert(invalidFour.flatMap(illegalF) === illegal(notOdd, wrongType))
    }

    it("should shortcut on illegal values") {
      assert(illegalInt.flatMap(validF) === illegalInt)
      assert(illegalInt.flatMap(invalidF) === illegalInt)
      assert(illegalInt.flatMap(illegalF) === illegalInt)
    }


  }


}
