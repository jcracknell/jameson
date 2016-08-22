package jameson
package enc

import org.scalatest.{FunSpec, Matchers}

class JStringEncoderSpec extends FunSpec with Matchers {
  import JStringEncoder._

  case class TestOptions(
    escapeNonASCII: Boolean = defaultOptions.escapeNonASCII,
    escapeQuotes: Boolean = defaultOptions.escapeQuotes,
    escapeSlashes: Boolean = defaultOptions.escapeSlashes
  ) extends Options

  describe("encode") {
    it("should escape quotes") {
      encode("\"") should be("\"\\\"\"")
    }
    it("should escape backslashes") {
      encode("\\") should be("\"\\\\\"")
    }
    it("should escape short escape sequences") {
      encode("\b") should be("\"\\b\"")
      encode("\t") should be("\"\\t\"")
      encode("\n") should be("\"\\n\"")
      encode("\r") should be("\"\\r\"")
      encode("\f") should be("\"\\f\"")
    }
    it("should escape in the middle of a string") {
      encode("ab\ncd") should be("\"ab\\ncd\"")
    }
    describe("options") {
      it("should escape non-ASCII characters when configured to do so") {
        encode("\u00e9", TestOptions()) should be("\"\u00e9\"")
        encode("\u00e9", TestOptions(escapeNonASCII = true)) should be("\"\\u00e9\"")
      }
      it("should escape single quotes when configured to do so") {
        encode("'", TestOptions()) should be("\"'\"")
        encode("'", TestOptions(escapeQuotes = true)) should be("\"\\u0027\"")
      }
      it("should escape forward slashes when configured to do so") {
        encode("/", TestOptions()) should be("\"/\"")
        encode("/", TestOptions(escapeSlashes = true)) should be("\"\\/\"")
      }
    }
  }
}
