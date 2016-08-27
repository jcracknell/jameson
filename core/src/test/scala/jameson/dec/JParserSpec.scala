package jameson
package dec

import java.io.StringReader

import org.scalatest.{FunSpec, Matchers}

import scala.language.implicitConversions

class JParserSpec extends FunSpec with Matchers {
  implicit def mkContext(str: String): JParsingContext =
    new JParsingContext(new java.io.StringReader(str), JPath, new Jameson.ParsingOptions())

  def parse(ctx: JParsingContext): JValue = JParser.parse(ctx)(_.copy())

  describe("parse") {
    it("should parse the empty object") {
      parse("{}") should be (JObject())
      parse("{ }") should be (JObject())
    }
    it("should parse an object with a single property") {
      parse("""{ "foo": "bar" }""") should be(JObject("foo" := JString("bar")))
    }
    it("should parse an object with multiple properties") {
      parse("""{ "a":1, "b":2, "c":3 }""") should be (JObject("a" := JNumber(1), "b" := JNumber(2), "c" := JNumber(3)))
    }
    it("should parse an object with any property type") {
      parse("""{
        "null": null, "true": true, "false": false, "number": 123, "string": "foo",
        "array": [1,2,3], "object": { "foo": "bar" }
      }""") should be (JObject(
        "null" := JNull,
        "true" := JTrue, "false" := JFalse,
        "number" := JNumber(123),
        "string" := JString("foo"),
        "array" := JArray(JNumber(1), JNumber(2), JNumber(3)),
        "object" := JObject("foo" := JString("bar"))
      ))
    }
    it("should parse the empty array") {
      parse("[]") should be (JArray())
      parse("[ ]") should be (JArray())
    }
    it("should parse an array with a single element") {
      parse("[1]") should be (JArray(JNumber(1)))
    }
    it("should parse an array with multiple elements") {
      parse("[1,2,3]") should be (JArray(JNumber(1), JNumber(2), JNumber(3)))
    }
    it("should parse an array with any element type") {
      parse("""[ null, true, false, 123, "foo", [1,2,3], { "foo": "bar" } ]""") should be (JArray(
        JNull,
        JTrue, JFalse,
        JNumber(123),
        JString("foo"),
        JArray(JNumber(1), JNumber(2), JNumber(3)),
        JObject("foo" := JString("bar"))
      ))
    }
  }
  describe("number") {
    it("should parse integer values") {
      JParser.number("0") should be (JNumber(0d))
      JParser.number("1") should be (JNumber(1d))
      JParser.number("2") should be (JNumber(2d))
      JParser.number("3") should be (JNumber(3d))
      JParser.number("4") should be (JNumber(4d))
      JParser.number("5") should be (JNumber(5d))
      JParser.number("6") should be (JNumber(6d))
      JParser.number("7") should be (JNumber(7d))
      JParser.number("8") should be (JNumber(8d))
      JParser.number("9") should be (JNumber(9d))
      JParser.number("123") should be (JNumber(123d))
    }
    it("should parse values with a decimal part") {
      JParser.number("0.1") should be (JNumber(0.1d))
      JParser.number("1.2") should be (JNumber(1.2d))
    }
    it("should parse values with an exponent part") {
      JParser.number("1e3") should be (JNumber(1e3d))
      JParser.number("1.2E3") should be (JNumber(1.2E3d))
      JParser.number("1.3e0") should be (JNumber(1.3e0d))
      JParser.number("1.4e02") should be (JNumber(1.4e02d))
      JParser.number("1.5E+3") should be (JNumber(1.5E+3d))
      JParser.number("1.6e-3") should be (JNumber(1.6E-3d))
    }
    it("should parse negative values") {
      JParser.number("-0") should be (JNumber(-0d))
      JParser.number("-1") should be (JNumber(-1d))
      JParser.number("-1.2") should be (JNumber(-1.2d))
      JParser.number("-2e3") should be (JNumber(-2e3))
      JParser.number("-2.3e4") should be (JNumber(-2.3e4))
    }
  }
  describe("whitespace") {
    it("should parse single line comments when configured to do so") {
      val input = """// foo bar baz"""
      val ctx = new JParsingContext(new StringReader(input), JPath, new Jameson.ParsingOptions(allowComments = true))

      ctx.charIndex should be (0)
      JParser.whitespace(ctx) should be (true)
      ctx.charIndex should not be (0)
      ctx.atEOF should be (true)
    }
    it("should parse multi-line comments when configured to do so") {
      val input = "/* foo \n bar */"
      val ctx = new JParsingContext(new StringReader(input), JPath, new Jameson.ParsingOptions(allowComments = true))

      ctx.charIndex should be (0)
      JParser.whitespace(ctx) should be (true)
      ctx.charIndex should not be (0)
      ctx.atEOF should be (true)
    }
  }
}
