package jameson
package dec

import org.scalatest.{FunSpec, Matchers}
import scala.language.implicitConversions

class JParsingContextSpec extends FunSpec with Matchers {
  implicit def mkContext(input: String): JParsingContext =
    new JParsingContext(new java.io.StringReader(input), JPath, new Jameson.ParsingOptions())

  describe("read: ()Int") {
    it("should work as expected") {
      val ctx = mkContext("abc")
      ctx.read() should be ('a'.toInt)
      ctx.read() should be ('b'.toInt)
      ctx.read() should be ('c'.toInt)
      ctx.read() should be (-1)
      ctx.atEOF should be (true)
    }
  }

  describe("read: (buffer: Array[Char], offset: Int, length: Int)Int") {
    it("should work as expected") {
      val input = (
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut " +
        "labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco " +
        "laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in " +
        "voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat " +
        "non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.\n\n"
      ) * 16

      val ctx = mkContext(input)

      // Here we use a bizarre buffer size to try and force various interesting scenarios to occur
      val buffer = Array.ofDim[Char](31)
      val sb = new java.lang.StringBuilder
      def loop(): String = {
        val n = ctx.read(buffer, 0, buffer.length)
        if(n <= 0) sb.toString else {
          sb.append(buffer, 0, n)
          loop()
        }
      }

      loop() should be (input)
      ctx.atEOF should be (true)
      ctx.charIndex should be (input.length)

      val lines = input.split("\n", -1)
      ctx.lineNumber should be (lines.length)
      ctx.columnIndex should be (lines(lines.length - 1).length)
    }
  }
}
