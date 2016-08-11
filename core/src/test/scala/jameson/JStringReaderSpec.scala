package jameson

import org.scalatest.{FunSpec, Matchers}

class JStringReaderSpec extends FunSpec with Matchers {
  def read(str: String): String = using(new JStringReader(new java.io.StringReader(str))) { reader =>
    val sb = new java.lang.StringBuilder
    val buffer = Array.ofDim[Char](1024)
    def loop(): String = {
      val n = reader.read(buffer, 0, buffer.length)
      if(n <= 0) sb.toString else {
        sb.append(buffer, 0, n)
        loop()
      }
    }
    loop()
  }

  it("should read short escape sequences") {
    read("\\b\"") should be ("\b")
    read("\\f\"") should be ("\f")
    read("\\n\"") should be ("\n")
    read("\\r\"") should be ("\r")
    read("\\t\"") should be ("\t")
  }
  it("should read unicode escape sequences") {
    read("\\u0000\"") should be ("\u0000")
    read("\\u0001\"") should be ("\u0001")
    read("\\u0002\"") should be ("\u0002")
    read("\\u0003\"") should be ("\u0003")
    read("\\u0004\"") should be ("\u0004")
    read("\\u0005\"") should be ("\u0005")
    read("\\u0006\"") should be ("\u0006")
    read("\\u0007\"") should be ("\u0007")
    read("\\u0008\"") should be ("\u0008")
    read("\\u0009\"") should be ("\u0009")
    read("\\u000a\"") should be ("\u000a")
    read("\\u000b\"") should be ("\u000b")
    read("\\u000c\"") should be ("\u000c")
    read("\\u000d\"") should be ("\u000d")
    read("\\u000e\"") should be ("\u000e")
    read("\\u000f\"") should be ("\u000f")
    read("\\u000A\"") should be ("\u000A")
    read("\\u000B\"") should be ("\u000B")
    read("\\u000C\"") should be ("\u000C")
    read("\\u000D\"") should be ("\u000D")
    read("\\u000E\"") should be ("\u000E")
    read("\\u000F\"") should be ("\u000F")
    read("\\u1000\"") should be ("\u1000")
    read("\\u0200\"") should be ("\u0200")
    read("\\u0030\"") should be ("\u0030")
    read("\\u0004\"") should be ("\u0004")
  }
  it("should throw an exception for an unterminated string") {
    intercept[JParsingException] { read("abc") }
  }
  it("should throw an exception for an invalid escape sequence") {
    intercept[JParsingException] { read("\\a\"") }
  }
  it("should throw an exception for a string containing a control character") {
    intercept[JParsingException] { read("z\u0003z\"")}
  }
}
