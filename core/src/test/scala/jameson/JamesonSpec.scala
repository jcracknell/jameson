package jameson

import org.scalatest.{FunSpec, Matchers}

class JamesonSpec extends FunSpec with Matchers {
  describe("encoding API") {
    it("should write JNull") {
      Jameson.write(JNull) should be ("""null""")
    }
    it("should write JBooleans") {
      Jameson.write(JTrue) should be ("""true""")
      Jameson.write(JFalse) should be ("""false""")
    }
    it("should write JStrings") {
      Jameson.write(JString("")) should be (""" "" """.trim)
      Jameson.write(JString("foo")) should be (""" "foo" """.trim)
    }
    it("should write JNumbers") {
      Jameson.write(JNumber(0)) should be ("0")
      Jameson.write(JNumber(1)) should be ("1")
      Jameson.write(JNumber(Int.MaxValue)) should be (Int.MaxValue.toString)
      Jameson.write(JNumber(Int.MinValue)) should be (Int.MinValue.toString)
      Jameson.write(JNumber(1.23)) should be ("1.23")
      Jameson.write(JNumber(0.12)) should be ("0.12")
      Jameson.write(JNumber(2e20)) should be ("2E20")
    }
    it("should write JObjects") {
      Jameson.write(JObject()) should be ("""{}""")
      Jameson.write(JObject("foo" -> JString("bar"))) should be ("""{"foo":"bar"}""")
      Jameson.write(JObject("1" -> JNumber(1.23), "2" -> JNumber(2.34))) should be ("""{"1":1.23,"2":2.34}""")
    }
    it("should write JArrays") {
      Jameson.write(JArray()) should be ("""[]""")
      Jameson.write(JArray(JString("foo"))) should be ("""["foo"]""")
    }
  }
}
