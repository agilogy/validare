package ut

import com.agilogy.validare.Context
import org.scalatest.FunSpec

class ContextTest extends FunSpec {

  describe("Context") {

    it("should have a Self") {
      assert(Context.Self.toString === ".")
      assert(Context.Self.parts === Seq(Context.Part(".")))
    }

    it("should compose with string") {
      assert(Context.Self / "" === Context.Self)
      val a = Context.Self / "a"
      assert(a.parts === Seq(Context.Part("a")))
      assert(a.toString === "a")
      val ab = a / "b"
      assert(ab.toString === "a/b")
    }

    it("should compose with int") {
      val one = Context.Self / 1
      assert(one.toString === ".[1]")
      assert(one.parts === Seq(Context.Part(".",1)))
      val oneTwo = one / 2
      assert(oneTwo.toString === ".[1][2]")
    }

    it("should compose with other contexts"){
      val ctx1 = Context.Self / "a" / 1
      val ctx2 = Context.Self / "foo" / "bar" / 2
      val res = ctx1 / ctx2
      assert(res.toString === "a[1]/foo/bar[2]")
    }

  }
}
