package ut

import com.agilogy.validare.incontext._
import org.scalatest.FunSpec

class ValidatedTest extends FunSpec {

  val noFoo = "no foo"
  val boom = "booom!"
  val validFoo: Validated[String] = Valid("foo")
  val invalidFoo: Validated[String] = invalid("foo", noFoo)
  val invalidFailures = invalidFoo.asInstanceOf[Invalid[String]].errors
  val illegalFoo: Validated[String] = illegal(boom)
  val illegalFailures = illegalFoo.asInstanceOf[Illegal].errors

  describe("fold") {

    it("should fold over valid values") {
      assert(validFoo.fold(_ => "boo!", _ + "bar") === "foobar")

    }

    it("should fold over invalid values") {
      assert(invalidFoo.fold(_(Context.Self).map(_.message), _ => "ouch!") === Seq(noFoo))
    }

    it("should fold over illegal values") {
      assert(illegalFoo.fold(_(Context.Self).map(_.message), _ => "ouch!") === Seq(boom))
    }
  }

  describe("isSuccess and isError") {

    it("should consider Valid a success") {
      assert(validFoo.isSuccess === true)
      assert(validFoo.isError === false)
    }

    it("should consider invalid an error") {
      assert(invalidFoo.isSuccess === false)
      assert(invalidFoo.isError === true)
    }

    it("should consider illegal an error") {
      assert(illegalFoo.isSuccess === false)
      assert(illegalFoo.isError === true)
    }

  }

  describe("foreach") {

    it("should execute side effects for Valid values only") {
      val sb = new StringBuffer()
      val log = {s:String => sb.append(s);()}
      invalidFoo.foreach(log)
      assert(sb.toString === "")
      illegalFoo.foreach(log)
      assert(sb.toString === "")
      validFoo.foreach(log)
      assert(sb.toString === "foo")
    }

  }

  describe("orElse") {

    it("should keep Valid values unchanged") {
      assert(validFoo.orElse(Valid("bar")) === validFoo)
    }

    it("should use the default value for invalid values") {
      assert(invalidFoo.orElse(Valid("bar")) === Valid("bar"))
    }

    it("should use the default value for illegal values") {
      assert(illegalFoo.orElse(Valid("bar")) === Valid("bar"))
    }

  }

  describe("getOrElse") {

    it("should return the valid value") {
      assert(validFoo.getOrElse("bar") === "foo")
    }

    it("should return the default value for invalid values") {
      assert(invalidFoo.getOrElse("bar") === "bar")
    }

    it("should return the default value for illegal values") {
      assert(illegalFoo.getOrElse("bar") === "bar")
    }

  }

  describe("exists") {


    def f(sb: StringBuffer): (String => Boolean) = {
      s =>
        sb.append("invoked!")
        s.startsWith("foo")
    }

    it("should check if the value matches the predicate for valid values") {
      val sb = new StringBuffer()
      assert(validFoo.exists(f(sb)) === true)
    }

    it("should return false directly for invalid or illegal values") {
      val sb = new StringBuffer()
      assert(invalidFoo.exists(f(sb)) === false)
      assert(illegalFoo.exists(f(sb)) === false)
      assert(sb.toString === "")
    }

  }

  describe("toEither") {

    it("should convert a valid value to right") {
      assert(validFoo.toEither === Right("foo"))
    }

    it("should convert a invalid and illegal values to left") {
      assert(invalidFoo.toEither === Left(invalidFailures))
      assert(illegalFoo.toEither === Left(illegalFailures))
    }

  }

  describe("toOption") {

    it("should convert a valid value to Some") {
      assert(validFoo.toOption === Some("foo"))
    }

    it("should convert a invalid and illegal values to None") {
      assert(invalidFoo.toOption === None)
      assert(illegalFoo.toOption === None)
    }

  }

  describe("combine") {

    val f: (String, String) => String = _ + _

    it("should return illegal whenever we have an illegal value, while accumulating errors") {
      assert(validFoo.combine(illegalFoo)(f) === illegalFoo)
      assert(illegalFoo.combine(validFoo)(f) === illegalFoo)

      assert(invalidFoo.combine(illegalFoo)(f) === illegal(noFoo, boom))
      assert(illegalFoo.combine(invalidFoo)(f) === illegal(boom, noFoo))

      assert(illegal("barboom!").combine(illegalFoo)(f) === illegal("barboom!", boom))
      assert(illegalFoo.combine(illegal("barboom!"))(f) === illegal(boom, "barboom!"))
    }

    it("should return invalid whenever we have some invalid value and no illegal value, while accumulating errors") {
      assert(validFoo.combine(invalid("bar", "no bar"))(f) === invalid("foobar", "no bar"))
      assert(invalid("bar", "no bar").combine(validFoo)(f) === invalid("barfoo", "no bar"))

      assert(invalidFoo.combine(invalid("bar", "no bar"))(f) === invalid("foobar", noFoo, "no bar"))
      assert(invalid("bar", "no bar").combine(invalidFoo)(f) === invalid("barfoo", "no bar", noFoo))
    }

    it("should return valid only if both validations are valid") {
      assert(validFoo.combine(Valid("bar"))(f) === Valid("foobar"))
      assert(Valid("bar").combine(validFoo)(f) === Valid("barfoo"))
    }

  }

  describe("flatMap") {

    val validF: String => Validated[String] = s => Valid(s + "!!")
    val invalidF: String => Validated[String] = s => invalid(s + "!!", "no f")
    val illegalF: String => Validated[String] = _ => illegal("f boom!")

    it("should apply the function to valid values") {
      assert(validFoo.flatMap(validF) === Valid("foo!!"))
      assert(validFoo.flatMap(invalidF) === invalid("foo!!", "no f"))
      assert(validFoo.flatMap(illegalF) === illegal("f boom!"))
    }

    it("should apply the function to invalid values and accumulate errors") {
      assert(invalidFoo.flatMap(validF) === invalid("foo!!", noFoo))
      assert(invalidFoo.flatMap(invalidF) === invalid("foo!!", noFoo, "no f"))
      assert(invalidFoo.flatMap(illegalF) === illegal(noFoo, "f boom!"))
    }

    it("should shortcut on illegal values") {
      assert(illegalFoo.flatMap(validF) === illegalFoo)
      assert(illegalFoo.flatMap(invalidF) === illegalFoo)
      assert(illegalFoo.flatMap(illegalF) === illegalFoo)
    }


  }


}
