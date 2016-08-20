package jameson

import org.scalatest.{FunSpec, Matchers}

class JValueSpec extends FunSpec with Matchers {
  describe("encoding API") {
    it("should encode JNull") {
      JValue.encode(JNull) should be ("""null""")
    }
    it("should encode JBooleans") {
      JValue.encode(JTrue) should be ("""true""")
      JValue.encode(JFalse) should be ("""false""")
    }
    it("should encode JStrings") {
      JValue.encode(JString("")) should be (""" "" """.trim)
      JValue.encode(JString("foo")) should be (""" "foo" """.trim)
    }
    it("should encode JNumbers") {
      //JValue.encode(JNumber(0)) should be ("""0""")
      JValue.encode(JNumber(1.23)) should be ("""1.23""")
    }
    it("should encode JObjects") {
      JValue.encode(JObject()) should be ("""{}""")
      JValue.encode(JObject("foo" -> JString("bar"))) should be ("""{"foo":"bar"}""")
      JValue.encode(JObject("1" -> JNumber(1.23), "2" -> JNumber(2.34))) should be ("""{"1":1.23,"2":2.34}""")
    }
    it("should encode JArrays") {
      JValue.encode(JArray()) should be ("""[]""")
      JValue.encode(JArray(JString("foo"))) should be ("""["foo"]""")
    }
  }
}
