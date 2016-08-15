package jameson

import org.scalatest.{FunSpec, Matchers}

class JPathSpec extends FunSpec with Matchers {
  describe("toString") {
    it("should correctly represent the base path") {
      JPath.toString should be (".")
    }
    it("should correctly represent indexes") {
      (JPath/0).toString should be (".[0]")
      (JPath/1).toString should be (".[1]")
    }
    it("should correctly represent properties") {
      (JPath/"a").toString should be (""".["a"]""")
      (JPath/"foo").toString should be (""".["foo"]""")
    }
    it("should correctly represent mixed paths") {
      (JPath/"foo"/0/"baz").toString should be (""".["foo"][0]["baz"]""")
    }
  }
}
