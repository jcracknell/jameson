package jameson

import org.scalatest.{FunSpec, Matchers}

class JamesonSpec extends FunSpec with Matchers {
  describe("encoding API") {
    it("should encode JNull") {
      Jameson.encode(JNull) should be ("""null""")
    }
    it("should encode JBooleans") {
      Jameson.encode(JTrue) should be ("""true""")
      Jameson.encode(JFalse) should be ("""false""")
    }
    it("should encode JStrings") {
      Jameson.encode(JString("")) should be (""" "" """.trim)
      Jameson.encode(JString("foo")) should be (""" "foo" """.trim)
    }
    it("should encode JNumbers") {
      //Jameson.encode(JNumber(0)) should be ("""0""")
      Jameson.encode(JNumber(1.23)) should be ("""1.23""")
    }
    it("should encode JObjects") {
      Jameson.encode(JObject()) should be ("""{}""")
      Jameson.encode(JObject("foo" -> JString("bar"))) should be ("""{"foo":"bar"}""")
      Jameson.encode(JObject("1" -> JNumber(1.23), "2" -> JNumber(2.34))) should be ("""{"1":1.23,"2":2.34}""")
    }
    it("should encode JArrays") {
      Jameson.encode(JArray()) should be ("""[]""")
      Jameson.encode(JArray(JString("foo"))) should be ("""["foo"]""")
    }
  }
}
