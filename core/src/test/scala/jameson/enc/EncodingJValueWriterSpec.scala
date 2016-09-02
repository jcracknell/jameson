package jameson
package enc

import org.scalatest.{FunSpec, Matchers}

class EncodingJValueWriterSpec extends FunSpec with Matchers {
  import EncodingJValueWriter._

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
