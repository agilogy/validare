package com.agilogy.path.ut

import com.agilogy.path.{Path, Segment}
import org.scalatest.FunSpec

class PathTest extends FunSpec {

  describe("Context") {

    it("should have a Self") {
      assert(Path.Self.toString === "")
      assert(Path.Self.segments === Seq())
    }

    it("should add string segments") {
      assert(Path.Self / "" === Path.Self)
      val a = Path.Self / "a"
      assert(a.segments === Seq(Segment("a")))
      assert(a.toString === "a")
      assert(a / "" === a)
      val ab = a / "b"
      assert(ab.toString === "a/b")
      assert(ab.segments === Seq(Segment("a"),Segment("b")))
    }

    it("should add int segments") {
      val one = Path.Self / 1
      assert(one.toString === "[1]")
      assert(one.segments === Seq(Segment(1)))
      val oneTwo = one / 2
      assert(oneTwo.toString === "[1][2]")
      assert(oneTwo.segments === Seq(Segment(1),Segment(2)))
    }

    it("should mix string and int segments"){
      val one = Path.Self / 1
      val res = one / "a" / 3
      assert(res.toString === "[1]/a[3]")
      assert(res.segments === Seq(Segment(1),Segment("a"),Segment(3)))
      val a = Path.Self / "a"
      val res2 = a / 3 / 4 / "b"
      assert(res2.toString === "a[3][4]/b")
      assert(res2.segments === Seq(Segment("a"),Segment(3),Segment(4), Segment("b")))
    }

    it("should prefix segments"){
      val path = Path.Self / "a" / "b" / 3 / "c"
      assert("omega" :: path === Path.Self / "omega" / "a" / "b" / 3 / "c")
      assert(3 :: path === Path.Self / 3 / "a" / "b" / 3 / "c")
    }

  }
}
