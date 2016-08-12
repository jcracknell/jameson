package jameson
package impl

import org.scalatest.{FunSpec, Matchers}

class JStringWriterSpec extends FunSpec with Matchers {
  def write(str: String, options: JStringWriter.Options = JStringWriter.defaultOptions): String = {
    val sw = new java.io.StringWriter
    using(new JStringWriter(sw, options)) { writer =>
      writer.write(str)
    }
    sw.toString
  }

  it("should escape quotes") {
    write("\"") should be ("\\\"")
  }
  it("should escape backslashes") {
    write("\\") should be ("\\\\")
  }
  it("should escape short escape sequences") {
    write("\b") should be ("\\b")
    write("\t") should be ("\\t")
    write("\n") should be ("\\n")
    write("\r") should be ("\\r")
    write("\f") should be ("\\f")
  }

  it("should escape in the middle of a string") {
    write("ab\ncd") should be ("ab\\ncd")
  }
  describe("options") {
    it("should not escape non-ASCII characters by default") {
      write("\u00e9") should be ("\u00e9")
    }
    it("should escape non-ASCII characters when configured to do so") {
      write("\u00e9", new JStringWriter.Options { def escapeNonASCII: Boolean = true }) should be ("\\u00e9")
    }
  }
}
