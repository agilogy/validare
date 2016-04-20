package ut

import com.agilogy.validare.{Context, Validity}
import org.scalatest.FunSpec

class ValidityTest extends FunSpec {

  import Validity._
  val self:Context = Context.Self

  describe("itShould"){

    it("should build a Validation failure without parameters"){
      assert(itShould("foo") === ValidationFailure("foo", Map.empty))
    }

    it("should build an Invalid with parameters"){
      assert(itShould("foo", "bar" -> "42", "zoo" -> "woa") === ValidationFailure("foo", Map("bar" -> "42", "zoo" -> "woa")))
    }

  }

  describe("Invalid.apply"){

    it("should build a Invalid with the default Self Context"){
      val vf1 = ValidationFailure("foo",Map.empty)
      val vf2 = ValidationFailure("bar",Map.empty)
      assert(Invalid(vf1,vf2) === Invalid(Map(self -> Seq(vf1,vf2))))
    }
  }

  describe("at"){

    it("should leave valid values unchanged"){
      assert(Valid.at(Context.Self / "ctx1") === Valid)
    }

    it("assign the context to the failures"){
      val vf1 = ValidationFailure("foo",Map.empty)
      val vf2 = ValidationFailure("bar",Map.empty)
      assert(Invalid(vf1,vf2).at(Context.Self / "a") === Invalid(Map((Context.Self / "a") -> Seq(vf1,vf2))))
    }

    it("should work with strings"){
      assert(Valid.at("a") === Valid)
      val inv = Invalid(itShould("foo"))
      assert(inv.at("a") === inv.at(Context.Self / "a"))
    }
  }

  describe("&&"){

    it("should && two successes"){
        assert((Valid && Valid) === Valid)
    }

    it("should && a success and a failure"){
      val inv = Invalid(itShould("foo"))
        assert((Valid && inv) === inv)
        assert((inv && Valid) === inv)
    }

    it("should combine two invalids combining failures of the same context"){
      val inv1 = itShould("foo")
      val inv2 = itShould("bar")
      assert((Invalid(inv1) && Invalid(inv2)) === Invalid(Map(self -> Seq(inv1,inv2))))
    }

    it("should combine two invalids combining failures of different contexts"){
      val inv1 = itShould("foo")
      val inv2 = itShould("bar")
      val ctxA = Context.Self / "a"
      val ctxB = Context.Self / "b"
      assert((Invalid(inv1).at(ctxA) && Invalid(inv2).at(ctxB)) === Invalid(Map(ctxA -> Seq(inv1), ctxB -> Seq(inv2))))
    }
  }

  describe("||"){

    val inv1 = Invalid(itShould("foo"))
    val inv2 = Invalid(itShould("bar"))

    it ("should honor the boolean algebra"){
      assert((Valid || Valid) == Valid)
      assert((Valid || inv1) == Valid)
      assert((inv1 || Valid) == Valid)
      //TODO: We should get information about both failures to validate
      assert((inv1 || inv2) == inv2)
      assert((inv2 || inv1) == inv1)
    }
  }

  describe("fold"){

    it("should fold over a validity"){
      assert(Valid.fold(_ => "noo!", "yeah") === "yeah")
      assert(Invalid(itShould("foo")).fold(_.issues(Context.Self).head.message, "yeah") === "foo")
    }
  }
}
