package ut

import com.agilogy.validare.{Valid, Validated}
import org.scalatest.FunSpec
import com.agilogy.validare._
import com.agilogy.validare.standalone._
import com.agilogy.path.Path.__

class ValidatedApplicativeTest extends FunSpec{

  import validatedApplicative._

  private val notFoo = itShould("be foo")
  private val notAString = itShould("be a string")
  private val validFoo: Validated[String] = Valid("foo")
  private val invalidFoo: Validated[String] = invalid("foo", __ -> notFoo)
  private val illegalFoo: Validated[String] = illegal(__ -> notAString)


  describe("mapN") {

    val noBar = itShould("not be bar")
    val invalidBar = invalid("bar", noBar)
    val noZoo = itShould("not be zoo")
    val invalidZoo = invalid("zoo", noZoo)
    val validZas = valid("zas")

    it("should map3") {
      assert(map3(validFoo.at("a"), invalidBar.at("b"), invalidZoo.at("c"))(_ + _ + _) === invalid("foobarzoo", __ / "b" -> noBar, __ / "c" -> noZoo))
    }

    it("should map4") {
      assert(map4(validFoo.at("a"), invalidBar.at("b"), invalidZoo.at("c"), validZas.at("d"))(_ + _ + _ + _) === invalid("foobarzoozas", __ / "b" -> noBar, __ / "c" -> noZoo))
    }

    it("should map5") {
      assert(map5(validFoo.at("a"), invalidBar.at("b"), invalidZoo.at("c"), validZas.at("d"), illegalFoo.at("e"))(_ + _ + _ + _ + _) === illegal( __ / "b" -> noBar, __ / "c" -> noZoo, __ / "e" -> notAString))
    }

    it("should map6") {
      assert(map6(validFoo.at("a"), invalidBar.at("b"), invalidZoo.at("c"), validZas.at("d"), invalidFoo.at("e"), illegalFoo.at("f"))(_ + _ + _ + _ + _ + _) === illegal(__ / "b" -> noBar, __ / "c" -> noZoo,  __ / "e" -> notFoo,  __ / "f" -> notAString))
    }
  }


}
