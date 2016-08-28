package jameson
package dec

import org.scalatest.{FunSpec, Matchers}

class ParsingJObjectReaderSpec extends FunSpec with Matchers {
  describe("capture") {
    Jameson.read(""" { "present": 1, "mismatched": null } """) {
      case reader: JObjectReader =>
        val present = reader.capture("present") { case JNumber(v) => v }
        val mismatched = reader.capture("mismatched") { case s: JStringReader => s.copy() }
        val missing = reader.capture("missing") { case JNull => null }
        reader.discard()

        it("should record the correct path and property name") {
          present.path should be (JPath/"present")
          present.name should be ("present")
        }

        it("should throw an exception when attempting to register a duplicate capture") {
          intercept[Exception] { reader.captureValue("present") }
        }

        it("should correctly represent properties that were present") {
          present.wasPresent should be (true)
          present.wasCaptured should be (true)
          present.get should be (1d)
        }

        it("should correctly represent properties that were mismatched") {
          mismatched.wasPresent should be (true)
          mismatched.wasCaptured should be (false)
          intercept[Exception] { mismatched.get }
        }

        it("should correctly represent properties that were missing") {
          missing.wasPresent should be (false)
          missing.wasCaptured should be (false)
          intercept[Exception] { missing.get }
        }

      case _ => ???
    }
  }
}
