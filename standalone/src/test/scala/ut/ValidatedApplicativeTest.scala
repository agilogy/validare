package ut

import org.scalatest.FunSpec
import com.agilogy.validare.standalone._
import validatedApplicative._

class ValidatedApplicativeTest extends FunSpec{

  val noFoo = "no foo"
  val boom = "booom!"
  val validFoo: Validated[String] = Valid("foo")
  val invalidFoo: Validated[String] = invalid("foo", noFoo)
  val illegalFoo: Validated[String] = illegal(boom)


  describe("mapN") {

    val v1 = invalid("bar", "no bar")
    val v2 = invalid("zoo", "no zoo")
    val v3 = valid("zas")

    it("should map3") {
      assert(map3(validFoo, v1, v2)(_ + _ + _) === invalid("foobarzoo", "no bar", "no zoo"))
    }

    it("should map4") {
      assert(map4(validFoo, v1, v2, v3)(_ + _ + _ + _) === invalid("foobarzoozas", "no bar", "no zoo"))
    }

    it("should map5") {
      assert(map5(validFoo, v1, v2, v3, illegalFoo)(_ + _ + _ + _ + _) === illegal("no bar", "no zoo", boom))
    }

    it("should map6") {
      assert(map6(validFoo, v1, v2, v3, invalidFoo, illegalFoo)(_ + _ + _ + _ + _ + _) === illegal("no bar", "no zoo", noFoo, boom))
    }
  }


}
