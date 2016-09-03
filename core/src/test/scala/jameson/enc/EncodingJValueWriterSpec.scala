package jameson
package enc

import org.scalatest.{FunSpec, Matchers}

class EncodingJValueWriterSpec extends FunSpec with Matchers {
  import EncodingJValueWriter._

  describe("encodeNumber") {
    it("should encode integers") {
      encodeNumber(0d) should be ("0")
      encodeNumber(1d) should be ("1")
      encodeNumber(-1d) should be ("-1")
      encodeNumber(Int.MaxValue.toDouble) should be (Int.MaxValue.toString)
      encodeNumber(Int.MinValue.toDouble) should be (Int.MinValue.toString)
    }
    it("should encode decimals") {
      encodeNumber(1.23d) should be ("1.23")
      encodeNumber(0.12d) should be ("0.12")
      encodeNumber(0.0101d) should be ("0.0101")
    }
    it("should encode scientific notation") {
      encodeNumber(1E20d) should be ("1E20")
      encodeNumber(1.23E20d) should be ("1.23E20")
      encodeNumber(1.0101E20) should be ("1.0101E20")
      encodeNumber(1E-20d) should be ("1E-20")
      encodeNumber(1.23E-20d) should be ("1.23E-20")
      encodeNumber(1.0101E-20) should be ("1.0101E-20")
    }
  }
  describe("encodeString") {
    it("should escape quotes") {
      encodeString("\"") should be("\"\\\"\"")
    }
    it("should escape backslashes") {
      encodeString("\\") should be("\"\\\\\"")
    }
    it("should escape short escape sequences") {
      encodeString("\b") should be("\"\\b\"")
      encodeString("\t") should be("\"\\t\"")
      encodeString("\n") should be("\"\\n\"")
      encodeString("\r") should be("\"\\r\"")
      encodeString("\f") should be("\"\\f\"")
    }
    it("should escape in the middle of a string") {
      encodeString("ab\ncd") should be("\"ab\\ncd\"")
    }
    describe("options") {
      it("should escape non-ASCII characters when configured to do so") {
        encodeString("\u00e9", new Jameson.EncodingOptions()) should be("\"\u00e9\"")
        encodeString("\u00e9", new Jameson.EncodingOptions(escapeNonASCII = true)) should be("\"\\u00e9\"")
      }
      it("should escape single quotes when configured to do so") {
        encodeString("'") should be("\"'\"")
        encodeString("'", new Jameson.EncodingOptions(escapeQuotes = true)) should be("\"\\u0027\"")
      }
      it("should escape forward slashes when configured to do so") {
        encodeString("/") should be("\"/\"")
        encodeString("/", new Jameson.EncodingOptions(escapeSlashes = true)) should be("\"\\/\"")
      }
    }
  }

}
